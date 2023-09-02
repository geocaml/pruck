type t
(** A dataframe *)

val columns : t -> int
(** The number of columns in [t] *)

val rows : t -> int
(** The number of rows in [t]. *)

module Value : sig
  type t

  val to_int : t -> int
  val to_float : t -> float
  val to_string : t -> string
  val to_bool : t -> bool
end

val get : t -> 'a array Column.column -> int -> 'a
(** [get df column idx] returns the element in [column] at
    index [idx]. *)

val get_value : t -> string -> int -> Data.Value.t
(** [get_value t name idx] is a bit like {! get} except it returns
    a packed value as we can't show the type. *)

val set : t -> 'a array Column.column -> int -> 'a -> unit
(** [set df column idx element] will set the element at index [idx]
    in [column] to [element]. *)

val empty : _ Column.columns -> t
(** Create an empty dataframe from a list of columns *)

val v : ('a, t) Column.columns -> 'a
(** [v columns args] creates a new dataframe from the typed columns and
    their values supplied as arrays. The type might seem confusing, but
    let's see an example.

    [{
      let df = Df.v [ age; height ] [| 24; 25 |] [| 1.81; 1.83 |]
    }]
*)

val read_csv : ?columns:_ Column.columns -> _ Eio.Path.t -> t
(** [read_csv ?columns path] will read [path] as a CSV file. If no
    [columns] are supplied then they will be inferred. If columns
    are supplied then only those columns will be read.

    @raises Failure If a column is specified that does not exist in the CSV. *)
