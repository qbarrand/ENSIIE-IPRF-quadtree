#use "quadtree.ml"

#use "display.ml"

(* Question 15 *)

(* Generates n objects with random coordinates within the QuadTree's 
 * rect boundaries.
 *)
let rec gen_objects_list = fun (r: rect) n calls ->
  if calls < n
  then let R(c1, c2) = r in
       let x = (Random.float (fst c2)) +. fst c1 in
       let y = (Random.float (snd c1)) +. snd c2 in
       ((x, y), Printf.sprintf "(%.2f, %.2f)" x y)::
	 gen_objects_list r n (calls + 1)
  else []
;;


(* Generates a random QuadTree using r's boundaries and containing n 
 * objects.
 *)
let gen_random_quadtree = fun r n ->
  let list = gen_objects_list r n 0 in
  quadtree_of_list list r
;;


(* Remove all objects contained into l from the q QuadTree.
 *)
let remove_objects_from_list = fun q l ->
  let remove_object =
    fun q item ->
    let c, _ = item in
    remove q c
  in 
  List.fold_left remove_object q l
;;

  
(* New simple_test : with two coordinates c1 and c2 and an int n,
 * creates a QuadTree, then inserts n random objects, then displays it,
 * then removes all previous inserted objects, then displays it again.
 *)
let new_simple_test = fun (c1: coord) (c2: coord) n ->
  let r = make_rect c1 c2 in
  (*  let list = gen_objects_list r n 0 in *)
  let qf = gen_random_quadtree r n in
  let list = list_of_quadtree qf in
  (* Display full QuadTree *)
  let _ = simple_test qf (fun str -> str) in
  let qe = remove_objects_from_list qf list in
  (* Display empty QuadTree *)
  let _ = simple_test qe (fun str -> str) in
  ()
;;


(* Question 16 *)

let rec bad_draw_quadtree = fun dparams data_to_string qt ->
  let sx,sy,z = dparams in
  let Q (r, qc) = qt in
  let x1 = int_of_float (sx +. z *. rect_left r) in
  let x2 = x1 + int_of_float (rect_length r) in
  let y1 = int_of_float (sy +. z *. rect_bottom r) in
  let y2 = y1 + int_of_float (rect_height r) in
  let _ = Graphics.set_color Graphics.blue in
  let _ = Graphics.draw_rect x1 y1 (x2-x1) (y2-y1) in
  let _ = Graphics.set_color Graphics.black in
  match qc with
    | Empty -> ()
    | Leaf (c,d) -> draw_data dparams data_to_string (c,d) Graphics.black
    | Node (nw,ne,se,sw) ->
       let _ = bad_draw_quadtree dparams data_to_string nw in
       let _ = bad_draw_quadtree dparams data_to_string ne in
       let _ = bad_draw_quadtree dparams data_to_string se in
       let _ = bad_draw_quadtree dparams data_to_string sw in
       ()
;;

let bad_simple_test = fun qt f ->
    let r = boundary qt in
    let dparams = init r in
    let _ = bad_draw_quadtree dparams f qt in
    wait_and_quit ()
;;

