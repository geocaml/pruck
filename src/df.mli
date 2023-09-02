type t
(** A dataframe *)

val columns : t -> int
(** The number of columns in [t] *)

val rows : t -> int
(** The number of rows in [t]. *)

exception Out_of_bounds

module Value : sig
  type t

  val to_int : t -> int
  val to_float : t -> float
  val to_string : t -> string
  val to_bool : t -> bool
end

val get : t -> 'a Column.column -> int -> 'a
(** [get df column idx] returns the element in [column] at
    index [idx]. *)

val get_value : t -> string -> int -> Data.Value.t
(** [get_value t name idx] is a bit like {! get} except it returns
    a packed value as we can't show the type. *)

val set : t -> 'a Column.column -> int -> 'a -> unit
(** [set df column idx element] will set the element at index [idx]
    in [column] to [element]. *)

val empty : _ Column.columns -> t
(** Create an empty dataframe from a list of columns *)

val v : column_names:string list -> _ array Data.rows -> t