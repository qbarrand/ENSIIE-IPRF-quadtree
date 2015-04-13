#use "quadtree.ml"

#use "display.ml"

(* Question 15 *)

let rec gen_qt_list =
  fun (r: rect) ->
  fun n ->
  fun calls ->
  if calls < n
  then match r with R(c1, c2) ->
		    let x = (Random.float (fst c2)) +. fst c1 in
		    let y = (Random.float (snd c1)) +. snd c2 in
		    ((x, y), Printf.sprintf "(%.2f, %.2f)" x y)::
		      gen_qt_list r n (calls + 1)
  else []
;;
  
let remove_objects_from_list =
  fun q ->
  fun l ->
  List.fold_left
    (fun q item ->
     match item with c, _ ->
       remove q c)
    q l
  ;;
    
let new_simple_test =
  fun (c1: coord) ->
  fun (c2: coord) ->
  fun n ->
  let r = make_rect c1 c2 in
  let list = gen_qt_list r n 0 in
  let qf = quadtree_of_list list r in
  (* Display full QuadTree *)
  let _ = simple_test qf (fun str -> str) in
  let qe = remove_objects_from_list qf list in
  (* Display empty QuadTree *)
  let _ = simple_test qe (fun str -> str) in
  ()
;;

new_simple_test (0., 0.) (100., 100.) 35 ;;
