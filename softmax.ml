let dot vecA vecB =
  let pro = Array.map2 ( *. ) vecA vecB in
  Array.fold_left ( +. ) 0.0 pro

let matdot matA vecB =
  Array.map (dot vecB) matA

let vecadd vecA vecB = Array.map2 ( +. ) vecA vecB

let softmax vec =
  let exp_vec = Array.map exp vec in
  let total = Array.fold_left ( +. ) 0.0 exp_vec in
  Array.map (fun x -> (exp x) /. total) vec

let logistic_regression ~weight ~bias inputs =
  softmax (vecadd (matdot weight inputs) bias)

let onehot length number =
  Array.init length (fun n -> if n == number then 1.0 else 0.0)

let error_grad y_calc y_true =
  Array.map2 ( -. ) y_calc y_true

let train_logistic weight bias data target =
  let batch_size = 10 in
  let y_true =
    let num_labels = Array.length bias in
    Array.map (onehot num_labels) target
  in
  let y_calc = Array.map (classifier ~weight:weight ~bias:bias) data in
  let g_calc = Array.map2 error_grad y_calc y_true in
  ()
;;

let test_logistic weight bias data target =
  ()
;;

let make_weight out_dim in_dim =
  Array.make_matrix out_dim in_dim 0.0

let make_bias out_dim =
  Array.make out_dim 0.0

let () =
  let mnist                     = Mnist.load_data in
  let train_data, train_target  = Hashtbl.find mnist "train" in
  let test_data, test_target    = Hashtbl.find mnist "test" in

  let weight      = make_weight Mnist.num_labels Mnist.image_dim in
  let bias        = make_bias   Mnist.num_labels in
  train_logistic weight bias train_data train_target;
  test_logistic  weight bias test_data  test_target;
;;
