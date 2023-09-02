Pruck
-----

A simple dataframe library in OCaml trying to strike a balance between the types and untyped world.

In order to have types for your column values, you will need to provide column names tagged with the kind
of elements you expect that column to contain.

```ocaml
open Pruck
let age = Column.Int "age"
let height = Column.Float "height"
```

From there you can create an empty dataframe yourself.

```ocaml
# let df = Df.empty Column.[ age; height ];;
val df : Df.t = <abstr>
# Df.rows df;;
- : int = 0
# Df.columns df;;
- : int = 2
```

Or you can fill it with values. Note, we get the kind of the values from the arrays with this
dataframe builder.

```ocaml
# let df = Df.v ~column_names:[ "age"; "height" ] Data.[ Int [| 24 |]; Float [| 1.81 |] ];;
val df : Df.t = <abstr>
# Df.get df age 0;;
- : int = 24
# Df.get df height 0;;
- : float = 1.81
# Df.get_value df "height" 0 |> Data.Value.to_float;;
- : float = 1.81
```
