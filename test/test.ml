open Eio
open Pruck

let ( / ) = Eio.Path.( / )

let () =
  Eio_main.run @@ fun env ->
  let cwd = Stdenv.cwd env in
  let _ = Parquet.of_file (cwd / "test.parquet") in
  let _ = Df.read_parquet (cwd / "test.parquet") in
  ()
