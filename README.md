Pruck
-----

*Status: WIP & Experimental*

A simple dataframe library in OCaml trying to strike a balance between the typed and untyped world.

## Prerequisits

Pruck currently depends on you having installed:

* csv
* eio
* eio_main

## Usage

In order to have types for your column values, you will need to provide column names tagged with the kind
of elements you expect that column to contain.

```ocaml
open Pruck
let age = Column.Int "age"
let height = Column.Float "height"

let with_cwd fn =
  Eio_main.run @@ fun env ->
  let cwd = Eio.Stdenv.cwd env in
  fn cwd

let ( / ) = Eio.Path.( / )
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

Or you can fill it with values, the arguments depend on the columns you supply.

```ocaml
# let df = Df.v [ age; height ] [| 24; 25 |] [| 1.81; 1.83 |];;
val df : Df.t = <abstr>
# Df.get df age 0;;
- : int = 24
# Df.get df height 1;;
- : float = 1.83
# Df.get_value df "height" 1 |> Data.Value.to_float;;
- : float = 1.83
```

## From files

Pruck supports reading from the following file formats:

 - CSV
 - Parquet (WIP)

For example, consider the following CSV file.

```sh
$ cat > file.csv <<EOF \
> age,height \
> 24,1.81 \
> 25,1.83 \
> EOF
```

From here we can read the CSV into a dataframe.

```ocaml
# let df =
  with_cwd @@ fun cwd ->
  Df.read_csv ~columns:[ age; height ] (cwd / "file.csv");;
val df : Df.t = <abstr>
# Df.get df age 1;;
- : int = 25
# Df.get df height 1;;
- : float = 1.83
```

You don't have to supply your column types upfront, but then the CSV will contain all of them.

```ocaml
# let df =
  with_cwd @@ fun cwd ->
  Df.read_csv (cwd / "file.csv");;
val df : Df.t = <abstr>
# Df.get df age 1;;
- : int = 25
# Df.get df height 1;;
- : float = 1.83
# Fmt.str "%a" Df.pp df;;
- : string =
"<2 columns x 2 rows>\nage : int, height : float\n24, 1.81\n25, 1.83"
```

Things will go wrong if you ask for a column that we cannot find.

```ocaml
# let df =
  with_cwd @@ fun cwd ->
  Df.read_csv ~columns:[ Column.String "name" ] (cwd / "file.csv");;
Exception: Failure "Missing column(s) in the CSV: [name : string]".
```

And of course you will get the same kind of error if we don't infer the same type.

```ocaml
# let df =
  with_cwd @@ fun cwd ->
  Df.read_csv ~columns:[ Column.Int "height" ] (cwd / "file.csv");;
Exception: Failure "Missing column(s) in the CSV: [height : int]".
```
