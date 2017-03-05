open Printf

let train_images = "train-images-idx3-ubyte"
let train_labels = "train-labels-idx1-ubyte"
let test_images = "t10k-images-idx3-ubyte"
let test_labels = "t10k-labels-idx1-ubyte"
let image_dim   = 784
let num_labels  = 10

let fetch_data =
  let download_file filename =
    let parent =
      "http://yann.lecun.com/exdb/mnist/"
    in
    if not (Sys.file_exists filename) then begin
      printf "Downloading %s ...\n" filename;
      flush stdout;
      let com = sprintf "wget %s%s > /dev/null 2>&1" parent filename in
      ignore (Sys.command com)
    end
  in
  let extract_file filename =
    if not (Sys.file_exists filename) then begin
      printf "Extracting %s ...\n" (filename^".gz");
      flush stdout;
      let com = sprintf "gzip -dc %s > %s" (filename^".gz") filename in
      ignore (Sys.command com)
    end
  in
  let open_file filename =
    download_file (filename^".gz");
    extract_file filename;
  in
  open_file train_images;
  open_file train_labels;
  open_file test_images;
  open_file test_labels;
;;

let load_data =
  fetch_data;

  let load_mnist name_images name_labels length =
    let chan_images = open_in_bin name_images in
    let chan_labels = open_in_bin name_labels in
    for i = 0 to 16-1 do
      ignore (input_byte chan_images);
    done;
    for i = 0 to 8-1 do
      ignore (input_byte chan_labels);
    done;

    let data_images   = Array.make_matrix length image_dim 0 in
    let target_labels = Array.make length 0 in
    for i = 0 to length-1 do
      target_labels.(i) <- input_byte chan_labels;
      for j = 0 to image_dim-1 do
        data_images.(i).(j) <- input_byte chan_images;
      done
    done;
    close_in chan_images;
    close_in chan_labels;

    (data_images, target_labels)
  in

  let data_train, target_train = load_mnist train_images train_labels 60000 in
  let data_test, target_test = load_mnist test_images test_labels 10000 in
  let mnist = Hashtbl.create 1 in
  List.iter (fun (kwd, tok) -> Hashtbl.add mnist kwd tok)
  [
    "train", (data_train, target_train);
    "test",  (data_test, target_test);
  ];

  mnist
;;

let () =
  let mnist = load_data in
  let train_data, train_target = Hashtbl.find mnist "train" in
  printf "%d\n" train_target.(10);
  for i = 0 to image_dim-1 do
    printf "%d\n" train_data.(10).(i)
  done
;;

