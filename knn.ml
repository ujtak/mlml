open Printf

let sub vecA vecB =
  Array.map2 (fun x y -> x -. y) vecA vecB

let norm vec =
  let square = fun x -> x *. x in
  sqrt (Array.fold_left (+.) 0.0 (Array.map square vec))

let dist vecA vecB = norm (sub vecA vecB)

let arange first last =
  let base = Array.init (last-first) (fun x -> x) in
  Array.map (fun x -> x + first) base

let argsort_int vec =
  let len = Array.length vec in
  let args = arange 0 len in
  let compare_vec x y = vec.(x) - vec.(y) in
  Array.sort compare_vec args;
  args

let argsort_float vec =
  let len = Array.length vec in
  let args = arange 0 len in
  let compare_vec x y = int_of_float (vec.(x) -. vec.(y)) in
  Array.sort compare_vec args;
  args

let k_elems k vec = Array.sub vec 0 k

let min_max vec =
  let sortee = Array.copy vec in
  let len = Array.length sortee in
  Array.sort compare sortee;
  (sortee.(0), sortee.(len-1))

let vote target args =
  let zeros n = Array.make n 0 in
  let min_vec, max_vec = min_max target in
  let cand = zeros (max_vec - min_vec + 1) in
  Array.iter (fun x -> cand.(target.(x)) <- cand.(target.(x)) + 1) args;
  let sorted_arg = argsort_int cand in
  sorted_arg.(max_vec - min_vec) + min_vec

let knn_classifier data target sample =
  let k = 5 in
  let norm_vec = Array.map (dist sample) data in
  let sorted_arg = argsort_float norm_vec in
  let k_arg = k_elems k sorted_arg in
  let result = vote target k_arg in
  result

let swap vec i j =
  let tmp = vec.(i) in
  vec.(i) <- vec.(j);
  vec.(j) <- tmp

let rand_shuffle rand arr =
  Array.iteri (fun i _ -> swap arr i rand.(i)) arr

let rand_gen len = Array.init len (fun i -> Random.int (i+1))

let same x y = if x == y then 1 else 0

let () =
  Random.init 224;

  let iris_data, iris_target = Iris.load_data in
  let rand_arg = rand_gen (Array.length iris_data) in
  rand_shuffle rand_arg iris_data;
  rand_shuffle rand_arg iris_target;

  let train_data    = Array.sub iris_data 0 120 in
  let train_target  = Array.sub iris_target 0 120 in
  let test_data     = Array.sub iris_data 120 30 in
  let test_target   = Array.sub iris_target 120 30 in

  let knn           = knn_classifier train_data train_target in
  let result        = Array.map knn test_data in
  let num_corr      = Array.fold_left (+) 0 (Array.map2 same result test_target) in
  let prob          = (float_of_int num_corr) /. 30.0 in

  printf "Accuracy: %f\n" prob
;;
