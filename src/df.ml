open Eio
module Data = Data

module DataMap = struct
  type 'a data = Nil : 'a data | Cons : 'a entry * 'b data -> 'b data
  and 'a entry = 'a Column.column * 'a Data.data

  type hidden = H : 'a Data.data -> hidden

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

  let length (Map data) =
    let rec loop : type a b. int -> _ data -> int =
     fun acc -> function Nil -> acc | Cons (_, es) -> loop (1 + acc) es
    in
    loop 0 data
end

type t = { data : DataMap.t }

let columns t = DataMap.length t.data

let rows t =
  let (DataMap.Map d) = t.data in
  match d with
  | DataMap.Nil -> assert false
  | DataMap.Cons ((_, v), _) -> Data.length v

let get (type a) t (col : a array Column.column) idx : a =
  let (DataMap.Map map) = t.data in
  let data = DataMap.lookup_exn col map in
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
  { data = DataMap.Map data }

let v columns =
  let rec f_aux : type f c. t -> (f, t) Column.columns -> f =
   fun t v ->
    match v with
    | [] -> t
    | (Column.Int _ as v) :: tl ->
        fun x ->
          let (DataMap.Map acc) = t.data in
          let v =
            { data = DataMap.Map (DataMap.Cons ((v, Data.Int x), acc)) }
          in
          f_aux v tl
    | (Column.Float _ as v) :: tl ->
        fun x ->
          let (DataMap.Map acc) = t.data in
          let v =
            { data = DataMap.Map (DataMap.Cons ((v, Data.Float x), acc)) }
          in
          f_aux v tl
    | (Column.String _ as v) :: tl ->
        fun x ->
          let (DataMap.Map acc) = t.data in
          let v =
            { data = DataMap.Map (DataMap.Cons ((v, Data.String x), acc)) }
          in
          f_aux v tl
    | (Column.Bool _ as v) :: tl ->
        fun x ->
          let (DataMap.Map acc) = t.data in
          let v =
            { data = DataMap.Map (DataMap.Cons ((v, Data.Bool x), acc)) }
          in
          f_aux v tl
  in
  f_aux { data = DataMap.(Map Nil) } columns

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

let data_of_array :
    type a. string array array -> int -> a Column.column -> a Data.data =
 fun arr idx -> function
  | Column.Int _ -> Data.Int (Array.map int_of_string arr.(idx))
  | Column.Float _ -> Data.Float (Array.map float_of_string arr.(idx))
  | Column.Bool _ -> Data.Bool (Array.map bool_of_string arr.(idx))
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
let read_csv ?columns path =
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
            | [] -> acc
            | (Column.Hidden c, idx) :: cs ->
                let data = data_of_array all idx c in
                collect (DataMap.Cons ((c, data), acc)) cs
          in
          { data = DataMap.Map (collect DataMap.Nil headers_to_use) })
