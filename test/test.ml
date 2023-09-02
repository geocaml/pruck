open Eio

let ( / ) = Eio.Path.( / )

let () =
  Eio_main.run @@ fun env ->
  let cwd = Stdenv.cwd env in
  Parquet.of_file (cwd / "test.parquet")
