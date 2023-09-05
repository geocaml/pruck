type 'a data =
  | Int : int array -> int array data
  | Float : float array -> float array data
  | String : string array -> string array data
  | Bool : bool array -> bool array data

module Value = struct
  type t =
    | Int : int -> t
    | Float : float -> t
    | String : string -> t
    | Bool : bool -> t

  let to_int = function Int i -> i | _ -> invalid_arg "Not an integer"
  let to_float = function Float i -> i | _ -> invalid_arg "Not a float"
  let to_string = function String i -> i | _ -> invalid_arg "Not a string"
  let to_bool = function Bool i -> i | _ -> invalid_arg "Not a bool"

  let pp ppf = function
    | Int i -> Fmt.int ppf i
    | Float i -> Fmt.float ppf i
    | String i -> Fmt.string ppf i
    | Bool i -> Fmt.bool ppf i
end

let to_value_array : type a. a data -> Value.t array = function
  | Int i -> Array.map (fun v -> Value.Int v) i
  | Float i -> Array.map (fun v -> Value.Float v) i
  | String i -> Array.map (fun v -> Value.String v) i
  | Bool i -> Array.map (fun v -> Value.Bool v) i

let get (type a) : a array data -> int -> a =
 fun v idx ->
  match v with
  | Int arr -> Array.get arr idx
  | Float arr -> Array.get arr idx
  | String arr -> Array.get arr idx
  | Bool arr -> Array.get arr idx

let get_value (type a) : a data -> int -> Value.t =
 fun v idx ->
  match v with
  | Int arr -> Value.Int (Array.get arr idx)
  | Float arr -> Value.Float (Array.get arr idx)
  | String arr -> Value.String (Array.get arr idx)
  | Bool arr -> Value.Bool (Array.get arr idx)

let set (type a) : a array data -> int -> a -> unit =
 fun v idx ->
  match v with
  | Int arr -> Array.set arr idx
  | Float arr -> Array.set arr idx
  | String arr -> Array.set arr idx
  | Bool arr -> Array.set arr idx

let length (type a) : a data -> int = function
  | Int arr -> Array.length arr
  | Float arr -> Array.length arr
  | String arr -> Array.length arr
  | Bool arr -> Array.length arr

let filter_index arr pred =
  let is = ref [] in
  for i = 0 to Array.length arr - 1 do
    if not (pred (Array.unsafe_get arr i)) then is := i :: !is
  done;
  List.rev !is

let unfilter_index (type a) : (a -> bool) -> a array data -> int list =
 fun pred -> function
  | Int arr -> filter_index arr pred
  | Float arr -> filter_index arr pred
  | String arr -> filter_index arr pred
  | Bool arr -> filter_index arr pred

type 'a rows = [] : 'a rows | ( :: ) : 'a array data * 'b rows -> 'b rows
