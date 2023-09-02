open Eio

(* So much Cstruct <-> Bytes copying :( *)
let transport_of_flow flow =
  object
    inherit Thrift.Transport.t
    val mutable _opened = true

    method close =
      Flow.close flow;
      _opened <- false

    method opn = ()
    method isOpen = _opened
    method flush = ()

    method read bytes off len =
      let buf = Cstruct.create len in
      let i = Flow.single_read flow buf in
      Cstruct.blit_to_bytes buf 0 bytes off i;
      i

    method write bytes off len =
      Flow.copy_string (Bytes.sub_string bytes off len) flow

    method write_string s off len = Flow.copy_string (String.sub s off len) flow
  end

let protocol flow = new TCompactProtocol.t (transport_of_flow flow)

module F = struct
  module Column = struct
    module Metadata = struct
      type t = Format_types.columnMetaData

      type type' = Format_types.Type.t =
        | BOOLEAN
        | INT32
        | INT64
        | INT96
        | FLOAT
        | DOUBLE
        | BYTE_ARRAY
        | FIXED_LEN_BYTE_ARRAY

      let pp_type ppf = function
        | BOOLEAN -> Fmt.string ppf "BOOLEAN"
        | INT32 -> Fmt.string ppf "INT32"
        | INT64 -> Fmt.string ppf "INT64"
        | INT96 -> Fmt.string ppf "INT96"
        | FLOAT -> Fmt.string ppf "FLOAT"
        | DOUBLE -> Fmt.string ppf "DOUBLE"
        | BYTE_ARRAY -> Fmt.string ppf "BYTE_ARRAY"
        | FIXED_LEN_BYTE_ARRAY -> Fmt.string ppf "FIXED_LEN_BYTE_ARRAY"

      let type' (t : t) = t#get_type
    end

    module Chunk = struct
      type t = Format_types.columnChunk

      let metadata (t : t) = t#get_meta_data
      let file_path (t : t) = t#get_file_path
    end
  end

  module RowGroup = struct
    type t = Format_types.rowGroup

    let total_byte_size (t : t) = t#get_total_byte_size
    let num_rows (t : t) = t#get_num_rows
    let columns (t : t) = Option.value ~default:[] t#get_columns
  end

  module File = struct
    type t = Format_types.fileMetaData

    let read ~off file =
      Path.with_open_out ~create:(`If_missing 0o644) file @@ fun flow ->
      let fd = Eio_unix.Resource.fd_opt flow |> Option.get in
      Eio_unix.Fd.use_exn "seeking" fd (fun fd ->
          ignore (Unix.lseek fd off SEEK_SET : int));
      Format_types.read_fileMetaData (protocol flow)

    let num_rows (t : t) = t#get_num_rows
    let row_groups (t : t) = Option.value ~default:[] t#get_row_groups
  end
end

let of_file path =
  let size = Path.size ~follow:false path in
  let metadata =
    Path.with_open_in path @@ fun flow ->
    let length_and_magic = Cstruct.create 8 in
    File.pread_exact
      ~file_offset:(Optint.Int63.of_int64 (Int64.sub size 8L))
      flow [ length_and_magic ];
    let length = Cstruct.LE.get_uint32 length_and_magic 0 in
    let magic = Cstruct.to_string ~off:4 ~len:4 length_and_magic in
    assert (magic = "PAR1");
    let meta_off =
      Int64.to_int @@ Int64.sub size Int64.(add (of_int32 length) 8L)
    in
    F.File.read ~off:meta_off path
  in
  let column_types =
    F.File.row_groups metadata
    |> List.map F.RowGroup.columns
    |> List.map (List.filter_map F.Column.Chunk.metadata)
    |> List.map (List.filter_map F.Column.Metadata.type')
  in
  Eio.traceln "Length %a"
    Fmt.(list @@ list ~sep:(Fmt.any ", ") F.Column.Metadata.pp_type)
    column_types
