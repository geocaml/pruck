type 'a column =
  | Int : string -> int column
  | Float : string -> float column
  | String : string -> string column
  | Bool : string -> bool column

type t = Hidden : _ column -> t

let equal (type a b) (a : a column) (b : b column) : (a, b) Type.eq option = match a, b with
  | Int s, Int s' -> if String.equal s s' then Some Type.Equal else None
  | Float s, Float s' -> if String.equal s s' then Some Type.Equal else None
  | String s, String s' -> if String.equal s s' then Some Type.Equal else None
  | Bool s, Bool s' ->if String.equal s s' then Some Type.Equal else None
  | _ -> None

let reconstruct (type a) (v : a column) (s : string) : a column = match v with
  | Int _ -> Int s
  | Float _ -> Float s
  | String _ -> String s
  | Bool _ -> Bool s

type 'a columns =
  | [] : 'a columns
  | (::) : 'a column * 'b columns -> 'b columns