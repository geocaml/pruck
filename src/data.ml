type 'a data =
  | Int : int array -> int array data
  | Float : float array -> float array data

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
end

let get (type a) : a array data -> int -> a = fun v idx -> match v with
  | Int arr -> Array.get arr idx
  | Float arr -> Array.get arr idx

let get_value (type a) : a array data -> int -> Value.t = fun v idx -> match v with
  | Int arr -> Value.Int (Array.get arr idx)
  | Float arr -> Value.Float (Array.get arr idx)

let set (type a) : a array data -> int -> a -> unit = fun v idx -> match v with
  | Int arr -> Array.set arr idx
  | Float arr -> Array.set arr idx

let length (type a) : a array data -> int = function
  | Int arr -> Array.length arr
  | Float arr -> Array.length arr

type 'a rows =
  | [] : 'a rows
  | (::) : 'a array data * 'b rows -> 'b rows