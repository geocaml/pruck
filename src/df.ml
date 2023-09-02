module Data = Data

exception Out_of_bounds

module DataMap = struct
  type 'a data =
    | Nil : 'a data
    | Cons : 'a entry * 'b data -> 'b data

  and 'a entry = 'a Column.column * 'a array Data.data

  type hidden = H : 'a array Data.data -> hidden

  let lookup_exn (type a) (col : a Column.column) data : a array Data.data =
    let rec lookup : type b c. b data -> a array Data.data = function
      | Nil -> raise Not_found
      | Cons ((c, data), cs) -> match Column.equal col c with
        | Some Type.Equal -> data
        | None -> lookup cs
    in
    lookup data

  let lookup_name col_name data =
    let rec lookup : type b c. b data -> hidden = function
      | Nil -> raise Not_found
      | Cons ((c, data), cs) ->
        let col = Column.reconstruct c col_name in
        match Column.equal col c with
        | Some Type.Equal -> H data
        | None -> lookup cs
    in
    lookup data

  type t = Map : _ data -> t

  let length (Map data) =
    let rec loop : type a b. int -> _ data -> int = fun acc -> function
     | Nil -> acc
     | Cons (_, es) -> loop (1 + acc) es
    in
    loop 0 data
end

type t = {
  data : DataMap.t;
}

let columns t = DataMap.length t.data

let rows t =
  let (DataMap.Map d) = t.data in
  match d with
  | DataMap.Nil -> assert false
  | DataMap.Cons ((_, v), _) -> Data.length v

let get (type a) t (col : a Column.column) idx : a =
  let (DataMap.Map map) = t.data in
  let data = DataMap.lookup_exn col map in
  Data.get data idx

module Value = Data.Value

let get_value t column_name idx : Value.t =
  let (DataMap.Map map) = t.data in
  match DataMap.lookup_name column_name map with
  | DataMap.H data -> Data.get_value data idx

let set (type a) t (col : a Column.column) idx (el : a) : unit =
  let (DataMap.Map map) = t.data in
  let data = DataMap.lookup_exn col map in
  Data.set data idx el

(* module Csv = struct
  let headers f =
    let fold_for_types acc (column_name, v) = match int_of_string_opt v with
      | Some _ -> Column.(Int column_name :: acc)
      | None ->
        match float_of_string_opt v with
        | Some _ -> Column.(Float column_name :: acc)
        | None -> match String.lowercase_ascii v with
          | "true" | "false" -> Column.(Bool column_name :: acc)
          | _ -> Column.(String column_name :: acc)
    in
    List.fold_left fold_for_types Column.[] f
end *)

let empty_data_of_column : type a. a Column.column -> a array Data.data = function
  | Int _ -> Data.Int (Array.make 0 0)
  | Float _ -> Data.Float (Array.make 0 0.)
  | _ -> failwith "TODO"

let empty columns =
  let rec loop : type a b. a Column.columns -> a DataMap.data -> a DataMap.data = fun c acc -> match c with
    | [] -> acc
    | e :: es -> loop es (DataMap.Cons ((e, empty_data_of_column e), acc))
  in
  let data = loop columns DataMap.Nil in
  { data = DataMap.Map data }

let datatype_to_column_name : type a. a array Data.data -> string -> a Column.column = fun d s -> match d with
  | Data.Int _ -> Column.Int s
  | Data.Float _ -> Column.Float s

let v ~column_names vs =
  let rec zip : type a b. a DataMap.data -> string list -> b array Data.rows ->  a DataMap.data = fun acc cs vs -> match cs, vs with
    | [], [] -> acc
    | c :: cs, v :: vs ->
      let acc = DataMap.Cons ((datatype_to_column_name v c, v), acc) in
      zip acc cs vs
    | _ -> failwith "Number of column names and data columns not equal!"
  in
  let data = zip DataMap.Nil column_names vs in
  { data = DataMap.Map data }


