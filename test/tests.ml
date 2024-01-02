open Pruck
open Eio

module C = struct
  let area_name = Column.String "Area name"
  let freq = Column.String "Frequency"
  let year_2016 = Column.Float "2016"
  let year_2017 = Column.Float "2017"
  let year_2018 = Column.Float "2018"
  let year_2019 = Column.Float "2019"
  let year_2020 = Column.Float "2020"
  let year_2021 = Column.Float "2021"
  let year_2022 = Column.Float "2022"

  let all =
    Column.
      [
        area_name;
        freq;
        year_2016;
        year_2017;
        year_2018;
        year_2019;
        year_2020;
        year_2021;
        year_2022;
      ]
end

let main path =
  let df = Df.read_csv ~fill_default:true ~columns:C.all path in
  Df.where df C.area_name (String.equal "Cambridge");
  Fmt.pr "%a" Df.pp df

let () =
  Eio_main.run @@ fun env ->
  let cwd = Stdenv.cwd env in
  main Path.(cwd / "gov-uk-cycling.csv")
