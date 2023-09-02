type 'a column =
| Int : string -> int array column
| Float : string -> float array column
| String : string -> string array column
| Bool : string -> bool array column

val pp_column : 'a column Fmt.t

val equal : 'a column -> 'b column -> ('a, 'b) Type.eq option
(** Whether or not two column names are equal along with their type too. *)

type t = Hidden : _ column -> t
(** The type of columns storing elements of kind ['a]. *)

val pp : t Fmt.t

val reconstruct : 'a column -> string -> 'a column

type ('a, 'ty) columns =
  | [] : ('ty, 'ty) columns
  | (::) : 'a column * ('b, 'ty) columns -> ('a -> 'b, 'ty) columns

val ( @ ) : ('s, 'm) columns -> ('m, 'e) columns -> ('s, 'e) columns