type 'a column =
  | Int : string -> int array column
  | Float : string -> float array column
  | String : string -> string array column
  | Bool : string -> bool array column

let default_value : type a. a array column -> a = function
  | Int _ -> Int.zero
  | Float _ -> Float.nan
  | String _ -> String.empty
  | Bool _ -> false

let pp_column : type a. a column Fmt.t =
 fun ppf -> function
  | Int i -> Fmt.pf ppf "%s : int" i
  | Float f -> Fmt.pf ppf "%s : float" f
  | String s -> Fmt.pf ppf "%s : string" s
  | Bool b -> Fmt.pf ppf "%s : bool" b

type t = Hidden : _ column -> t

let pp ppf (Hidden c) = pp_column ppf c

let equal (type a b) (a : a column) (b : b column) : (a, b) Type.eq option =
  match (a, b) with
  | Int s, Int s' -> if String.equal s s' then Some Type.Equal else None
  | Float s, Float s' -> if String.equal s s' then Some Type.Equal else None
  | String s, String s' -> if String.equal s s' then Some Type.Equal else None
  | Bool s, Bool s' -> if String.equal s s' then Some Type.Equal else None
  | _ -> None

let reconstruct (type a) (v : a column) (s : string) : a column =
  match v with
  | Int _ -> Int s
  | Float _ -> Float s
  | String _ -> String s
  | Bool _ -> Bool s

type ('a, 'ty) columns =
  | [] : ('ty, 'ty) columns
  | ( :: ) : 'a column * ('b, 'ty) columns -> ('a -> 'b, 'ty) columns

let rec ( @ ) :
    type start s m e. (s, m) columns -> (m, e) columns -> (s, e) columns =
 fun l r -> match l with [] -> r | a :: q -> a :: (q @ r)
