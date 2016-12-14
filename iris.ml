open Printf

exception Invalid_data

let dataset = "iris.data"

let download_file filename =
  let parent  =
    "http://archive.ics.uci.edu/ml/machine-learning-databases/iris/"
  in
  if not (Sys.file_exists filename) then begin
    printf "Downloading %s ...\n" filename; flush stdout;
    let com =
      sprintf "wget %s%s > /dev/null 2>&1; sed -i '' -e '/^$/d' %s"
        parent filename filename
    in
    ignore (Sys.command com);
  end
;;

let parse_csv file =
  let parse_line str = Str.split (Str.regexp ",") str in
  let in_chan = open_in file in
  let rec str_iter chan =
    try
      let line = input_line chan in
      (parse_line line) :: str_iter in_chan
    with
      End_of_file -> []
  in
  str_iter in_chan
;;

let load_data =
  download_file dataset;
  let data = parse_csv dataset in
  let parse_iris = function
    | [] -> ([||], -1)
    | d1 :: d2 :: d3 :: d4 :: t :: [] ->
        let num_target = match t with
          | "Iris-setosa"     -> 0
          | "Iris-versicolor" -> 1
          | "Iris-virginica"  -> 2
          | _                 -> -1
        in
        let num_data = Array.map float_of_string [|d1; d2; d3; d4|] in
        (num_data, num_target)
    | _ -> raise Invalid_data
  in
  let list_data, list_target = List.split (List.map parse_iris data) in
  let array_data    = Array.of_list list_data in
  let array_target  = Array.of_list list_target in
  (array_data, array_target)
;;

