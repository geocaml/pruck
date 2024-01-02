open Eio
module Data = Data

module Bitmask : sig
  type t

  val create : int -> t
  val count : t -> int
  val unset : t -> int -> unit
  val all_set : t -> int list

  (* val set : t -> int -> unit *)
  val get : t -> int -> bool
end = struct
  type t = { b : bytes; size : int; mutable set : int }

  let count t = t.set

  let create i =
    if i < 0 then invalid_arg "Bit mask must be greater than or equal to 0";
    let n = (i / 8) + 1 in
    let b = Bytes.create n in
    Bytes.fill b 0 n (Char.chr 255);
    { b; size = i; set = i }

  let unset t i =
    (* i / 8 *)
    let v = t.b in
    let b = i lsr 3 in
    if b > Bytes.length v then invalid_arg "Out of bounds bitmask"
    else
      let byte = Bytes.get_int8 v b in
      (* i mod 8 *)
      let r = i land 7 in
      let new_b = byte land lnot (1 lsl r) in
      Bytes.set_int8 v b new_b;
      t.set <- t.set - 1

  (* let set t i =
     (* i / 8 *)
     let v = t.b in
     let b = i lsr 3 in
     if b > Bytes.length v then invalid_arg "Out of bounds bitmask"
     else begin
       let byte = Bytes.get_int8 v b in
       (* i mod 8 *)
       let r = i land 7 in
       let new_b = byte lor (1 lsl r) in
       Bytes.set_int8 v b new_b;
       t.set <- t.set + 1
     end *)

  let get t i =
    (* i / 8 *)
    let t = t.b in
    let b = i lsr 3 in
    if b > Bytes.length t then invalid_arg "Out of bounds bitmask"
    else
      let r = Bytes.get_int8 t b in
      let mask = 1 lsl (i land 7) in
      r land mask <> 0

  let all_set t =
    let r = ref [] in
    for idx = 0 to t.size - 1 do
      if get t idx then r := idx :: !r
    done;
    List.rev !r
end

module DataMap = struct
  type 'a data = Nil : 'a data | Cons : 'a entry * 'b data -> 'b data
  and 'a entry = 'a Column.column * 'a Data.data

  type hidden = H : 'a Data.data -> hidden

  let reverse d =
    let r = ref Nil in
    let rec loop = function
      | Nil -> ()
      | Cons (e, es) ->
          r := Cons (e, !r);
          loop es
    in
    loop d;
    !r

  let lookup_exn (type a) (col : a Column.column) data : a Data.data =
    let rec lookup : type b c. b data -> a Data.data = function
      | Nil -> raise Not_found
      | Cons ((c, data), cs) -> (
          match Column.equal col c with
          | Some Type.Equal -> data
          | None -> lookup cs)
    in
    lookup data

  let lookup_name col_name data =
    let rec lookup : type b c. b data -> hidden = function
      | Nil -> raise Not_found
      | Cons ((c, data), cs) -> (
          let col = Column.reconstruct c col_name in
          match Column.equal col c with
          | Some Type.Equal -> H data
          | None -> lookup cs)
    in
    lookup data

  type t = Map : _ data -> t

  let length_data data =
    let rec loop : type a b. int -> _ data -> int =
     fun acc -> function Nil -> acc | Cons (_, es) -> loop (1 + acc) es
    in
    loop 0 data

  let length (Map data) = length_data data

  let columns data =
    let rec loop acc = function
      | Nil -> List.rev acc
      | Cons ((c, _), rest) -> loop (Column.Hidden c :: acc) rest
    in
    loop [] data

  let values data =
    (* The accumulator with list might atually be better :S *)
    let arr = Array.make (length_data data) [||] in
    let rec loopi i = function
      | Nil -> ()
      | Cons ((_, v), rest) ->
          let vs = Data.to_value_array v in
          arr.(i) <- vs;
          loopi (i + 1) rest
    in
    loopi 0 data;
    arr
end

type t = { data : DataMap.t; index_mask : Bitmask.t }

let columns t = DataMap.length t.data

let interal_rows t =
  (* DOES NOT LOOK AT THE BITMASK! *)
  let (DataMap.Map d) = t.data in
  match d with
  | DataMap.Nil -> assert false
  | DataMap.Cons ((_, v), _) -> Data.length v

let rows t = Bitmask.count t.index_mask

let get (type a) t (col : a array Column.column) idx : a =
  let (DataMap.Map map) = t.data in
  let data = DataMap.lookup_exn col map in
  assert (Bitmask.get t.index_mask idx);
  Data.get data idx

module Value = Data.Value

let get_value t column_name idx : Value.t =
  let (DataMap.Map map) = t.data in
  match DataMap.lookup_name column_name map with
  | DataMap.H data -> Data.get_value data idx

let set (type a) t (col : a array Column.column) idx (el : a) : unit =
  let (DataMap.Map map) = t.data in
  let data = DataMap.lookup_exn col map in
  Data.set data idx el

let where (type a) t (col : a array Column.column) (pred : a -> bool) : unit =
  let (DataMap.Map map) = t.data in
  let data = DataMap.lookup_exn col map in
  let indices = Data.unfilter_index pred data in
  List.iter (Bitmask.unset t.index_mask) indices

let empty_data_of_column : type a. a Column.column -> a Data.data = function
  | Int _ -> Data.Int (Array.make 0 0)
  | Float _ -> Data.Float (Array.make 0 0.)
  | String _ -> Data.String (Array.make 0 "")
  | Bool _ -> Data.Bool (Array.make 0 false)

let empty columns =
  let rec loop :
      type a b c. (b, c) Column.columns -> a DataMap.data -> a DataMap.data =
   fun c acc ->
    match c with
    | [] -> acc
    | e :: es -> loop es (DataMap.Cons ((e, empty_data_of_column e), acc))
  in
  let data = loop columns DataMap.Nil in
  { data = DataMap.Map data; index_mask = Bitmask.create 0 }

let v columns =
  let length = ref (-1) in
  let set_length l =
    if Int.equal !length (-1) then length := l
    else if not (Int.equal !length l) then invalid_arg "Mismatch in data length"
    else ()
  in
  let rec f_aux : type f c. t -> (f, t) Column.columns -> f =
   fun t v ->
    match v with
    | [] -> t
    | (Column.Int _ as v) :: tl ->
        fun x ->
          let (DataMap.Map acc) = t.data in
          set_length (Array.length x);
          let index_mask = Bitmask.create !length in
          let v =
            {
              data = DataMap.Map (DataMap.Cons ((v, Data.Int x), acc));
              index_mask;
            }
          in
          f_aux v tl
    | (Column.Float _ as v) :: tl ->
        fun x ->
          let (DataMap.Map acc) = t.data in
          set_length (Array.length x);
          let index_mask = Bitmask.create !length in
          let v =
            {
              data = DataMap.Map (DataMap.Cons ((v, Data.Float x), acc));
              index_mask;
            }
          in
          f_aux v tl
    | (Column.String _ as v) :: tl ->
        fun x ->
          let (DataMap.Map acc) = t.data in
          set_length (Array.length x);
          let index_mask = Bitmask.create !length in
          let v =
            {
              data = DataMap.Map (DataMap.Cons ((v, Data.String x), acc));
              index_mask;
            }
          in
          f_aux v tl
    | (Column.Bool _ as v) :: tl ->
        fun x ->
          let (DataMap.Map acc) = t.data in
          set_length (Array.length x);
          let index_mask = Bitmask.create !length in
          let v =
            {
              data = DataMap.Map (DataMap.Cons ((v, Data.Bool x), acc));
              index_mask;
            }
          in
          f_aux v tl
  in
  let index_mask = Bitmask.create 0 in
  f_aux { data = DataMap.(Map Nil); index_mask } columns

let transpose_arr vs =
  let col_length = Array.length vs in
  let cols = Array.length vs.(0) in
  let arr = Array.make_matrix cols col_length (Data.Value.Int 0) in
  Array.iteri
    (fun r row -> Array.iteri (fun c col -> arr.(c).(r) <- col) row)
    vs;
  arr

let pp ppf t =
  let r = rows t in
  let c = columns t in
  let (DataMap.Map data) = t.data in
  let cols = DataMap.columns data in
  let vs = DataMap.values data |> transpose_arr in
  let is = Bitmask.all_set t.index_mask in
  let vs =
    let off = ref 0 in
    let vals = Array.make (List.length is) [||] in
    Array.iteri
      (fun i row ->
        if List.mem i is then (
          vals.(!off) <- row;
          incr off)
        else ())
      vs;
    vals
  in
  Fmt.pf ppf "<%i columns x %i rows>@.%a@.%a" c r
    Fmt.(list ~sep:(Fmt.any ", ") Column.pp)
    cols
    Fmt.(array (array ~sep:(Fmt.any ", ") Value.pp))
    vs

let parse_csv_line s =
  (* TODO: this probably allocates a lot *)
  Csv.of_string s |> Csv.input_all |> Csv.to_array |> Array.map Array.to_list
  |> fun v -> Array.get v 0

let headers f =
  let rec fold :
      type a b c. Column.t list -> (string * string) list -> Column.t list =
   fun acc -> function
    | [] -> List.rev acc
    | (column_name, v) :: rest -> (
        match int_of_string_opt v with
        | Some _ ->
            let i = Column.Int column_name in
            fold (Column.Hidden i :: acc) rest
        | None -> (
            match float_of_string_opt v with
            | Some _ ->
                let i = Column.Float column_name in
                fold (Column.Hidden i :: acc) rest
            | None -> (
                match String.lowercase_ascii v with
                | "true" | "false" ->
                    let i = Column.Bool column_name in
                    fold (Column.Hidden i :: acc) rest
                | _ ->
                    let i = Column.String column_name in
                    fold (Column.Hidden i :: acc) rest)))
  in
  fold [] f

let transpose (vs : string list list) =
  let col_length = List.length vs in
  let cols = List.length (List.hd vs) in
  let arr = Array.make_matrix cols col_length "" in
  List.iteri (fun r row -> List.iteri (fun c col -> arr.(c).(r) <- col) row) vs;
  arr

let convert ?(fill_default = false) v conv arg =
  try conv arg
  with Failure msg ->
    if fill_default then Column.default_value v
    else Fmt.failwith "%s failed (ctx: trying to parse %s)" msg arg

let data_of_array :
    type a.
    ?fill_default:bool ->
    string array array ->
    int ->
    a Column.column ->
    a Data.data =
 fun ?fill_default arr idx v ->
  match v with
  | Column.Int _ ->
      Data.Int (Array.map (convert ?fill_default v int_of_string) arr.(idx))
  | Column.Float _ ->
      Data.Float (Array.map (convert ?fill_default v float_of_string) arr.(idx))
  | Column.Bool _ ->
      Data.Bool (Array.map (convert ?fill_default v bool_of_string) arr.(idx))
  | Column.String _ -> Data.String arr.(idx)

let rec column_exists :
    type a b c. int -> c Column.column -> Column.t list -> int option =
 fun idx c -> function
  | [] -> None
  | Column.Hidden c' :: cs -> (
      match Column.equal c c' with
      | None -> column_exists (idx + 1) c cs
      | Some Type.Equal -> Some idx)

(* TODO: It would be nice to use the CSV library directly but it uses bytes :( *)
let read_csv ?fill_default ?columns path =
  Path.with_open_in path @@ fun flow ->
  let lines = Buf_read.lines (Buf_read.of_flow ~max_size:max_int flow) in
  match lines () with
  | Seq.Nil -> failwith "No header in CSV file"
  | Seq.Cons (header, rest) -> (
      match rest () with
      | Seq.Nil -> failwith "No data in CSV file"
      | Seq.Cons (first_line, rest) ->
          let zipped =
            List.combine (parse_csv_line header) (parse_csv_line first_line)
          in
          let headers = headers zipped in
          let headers_to_use =
            match columns with
            | Some columns ->
                let found, not_found =
                  let rec idx :
                      type a b.
                      (Column.t * int option) list ->
                      (a, b) Column.columns ->
                      (Column.t * int option) list =
                   fun acc -> function
                    | [] -> List.rev acc
                    | c :: cs ->
                        let v = column_exists 0 c headers in
                        idx ((Column.Hidden c, v) :: acc) cs
                  in
                  let is = idx [] columns in
                  let found =
                    List.filter_map
                      (function c, Some v -> Some (c, v) | _ -> None)
                      is
                  in
                  let not_found =
                    List.filter_map
                      (function c, None -> Some c | _, Some _ -> None)
                      is
                  in
                  (found, not_found)
                in
                if not_found <> [] then
                  Fmt.failwith "Missing column(s) in the CSV: [%a]"
                    Fmt.(list ~sep:(Fmt.any ", ") Column.pp)
                    not_found
                else found
            | None -> List.mapi (fun i v -> (v, i)) headers
          in
          let all =
            first_line :: List.of_seq rest
            |> List.map parse_csv_line |> transpose
          in
          let rec collect acc = function
            | [] -> DataMap.reverse acc
            | (Column.Hidden c, idx) :: cs ->
                let data = data_of_array ?fill_default all idx c in
                collect (DataMap.Cons ((c, data), acc)) cs
          in
          let data = DataMap.Map (collect DataMap.Nil headers_to_use) in
          let v = { data; index_mask = Bitmask.create 0 } in
          let len = interal_rows v in
          { data; index_mask = Bitmask.create len })
