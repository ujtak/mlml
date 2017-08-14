open Printf

let inner vecA vecB =
  let pro = Array.map2 ( *. ) vecA vecB in
  Array.fold_left ( +. ) 0.0 pro

let outer vecA vecB =
  let scalar vecA b = Array.map (fun a -> a *. b) vecA in
  Array.map (scalar vecA) vecB

let vecadd vecA vecB = Array.map2 ( +. ) vecA vecB
let vecsub vecA vecB = Array.map2 ( -. ) vecA vecB
let matadd matA matB = Array.map2 vecadd matA matB
let matsub matA matB = Array.map2 vecsub matA matB

let matdot matA vecB = Array.map (inner vecB) matA

let vecsca c vec = Array.map (( *. ) c) vec
let matsca c mat = Array.map (vecsca c) mat

let softmax vec =
  let exp_vec = Array.map exp vec in
  let total = Array.fold_left ( +. ) 0.0 exp_vec in
  Array.map (fun x -> (exp x) /. total) vec

let logistic ~weight ~bias inputs =
  softmax (vecadd (matdot weight inputs) bias)

let onehot length number =
  Array.init length (fun n -> if n == number then 1.0 else 0.0)

let same x y =
  if x == y then 1 else 0

let label length target =
  let rec find max idx lbl vec =
    if idx == length then
      lbl
    else
      if vec.(idx) > max then
        find vec.(idx) (idx+1) idx vec
      else
        find max (idx+1) lbl vec
  in
  find target.(0) 0 0 target

let error_grad y_calc y_true = Array.map2 ( -. ) y_calc y_true
let weight_grad input grad = outer input grad
let bias_grad input grad = grad

let make_params out_dim in_dim value =
  let weight = Array.make_matrix out_dim in_dim value in
  let bias   = Array.make out_dim value in
  (weight, bias)



let test_logistic weight bias data target =
  let ys_calc =
    let classifier = logistic ~weight:weight ~bias:bias in
    Array.map classifier data
  in

  let label_len = Array.length bias in
  let result = Array.map (label label_len) ys_calc in
  let num_corr  = Array.fold_left (+) 0 (Array.map2 same result target) in

  let total_len  = Array.length target in
  let accuracy = (float_of_int num_corr) /. (float_of_int total_len) in
  accuracy
;;



let rec train_logistic epoch weight bias data target =
  let alpha = 0.001 in
  let total_len = Array.length target in
  let r_len = 1.0 /. (float_of_int total_len) in

  let ys_true =
    let onehot_label = onehot (Array.length bias) in
    Array.map onehot_label target
  in
  let ys_calc =
    let classifier = logistic ~weight:weight ~bias:bias in
    Array.map classifier data
  in

  let gs_calc   = Array.map2 error_grad ys_calc ys_true in
  let gs_weight = Array.map2 weight_grad data gs_calc in
  let gs_bias   = Array.map2 bias_grad data gs_calc in

  let init_weight, init_bias =
    let in_dim  = Array.length weight.(0) in
    let out_dim = Array.length weight in
    make_params out_dim in_dim 0.0
  in
  let g_weight  = matsca r_len (Array.fold_left matadd init_weight gs_weight) in
  let g_bias    = vecsca r_len (Array.fold_left vecadd init_bias   gs_bias  ) in

  let n_weight = matsub weight (matsca alpha g_weight) in
  let n_bias   = vecsub bias   (vecsca alpha g_bias) in

  if epoch == 1 then
    (n_weight, n_bias)
  else
    train_logistic (epoch-1) n_weight n_bias data target
;;



let () =
  let mnist                     = Mnist.load_data in
  let train_data, train_target  = Hashtbl.find mnist "train" in
  let test_data, test_target    = Hashtbl.find mnist "test" in
  let init_weight, init_bias =
    make_params Mnist.num_labels Mnist.image_dim 0.0
  in

  let weight, bias =
    train_logistic 1 init_weight init_bias train_data train_target
  in

  let accuracy =
    test_logistic weight bias test_data test_target
  in

  printf "Accuracy: %f\n" accuracy
;;
