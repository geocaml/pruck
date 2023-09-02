type 'a column =
| Int : string -> int column
| Float : string -> float column
| String : string -> string column
| Bool : string -> bool column

val equal : 'a column -> 'b column -> ('a, 'b) Type.eq option
(** Whether or not two column names are equal along with their type too. *)

type t = Hidden : _ column -> t
(** The type of columns storing elements of kind ['a]. *)

val reconstruct : 'a column -> string -> 'a column

type 'a columns =
    | [] : 'a columns
    | (::) : 'a column * 'b columns -> 'b columns