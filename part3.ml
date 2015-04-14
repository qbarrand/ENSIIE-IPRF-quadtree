#use "quadtree.ml"

#use "display.ml"

(* Question 15 *)

let rec gen_objects_list =
  fun (r: rect) ->
  fun n ->
  fun calls ->
  if calls < n
  then match r with R(c1, c2) ->
		    let x = (Random.float (fst c2)) +. fst c1 in
		    let y = (Random.float (snd c1)) +. snd c2 in
		    ((x, y), Printf.sprintf "(%.2f, %.2f)" x y)::
		      gen_objects_list r n (calls + 1)
  else []
;;

  
let remove_objects_from_list =
  fun q ->
  fun l ->
  let remove_object =
    fun q item ->
    match item with c, _ ->
      remove q c
  in 
  List.fold_left remove_object q l
;;
  

let new_simple_test =
  fun (c1: coord) ->
  fun (c2: coord) ->
  fun n ->
  let r = make_rect c1 c2 in
  let list = gen_objects_list r n 0 in
  let qf = gen_random_quadtree r n in
  (* Display full QuadTree *)
  let _ = simple_test qf (fun str -> str) in
  let qe = remove_objects_from_list qf list in
  (* Display empty QuadTree *)
  let _ = simple_test qe (fun str -> str) in
  ()
;;


(* Question 16 *)

let rec draw_quadtree = fun dparams data_to_string qt ->
  let sx,sy,z = dparams in
  let Q (r, qc) = qt in
  let x1 = int_of_float (sx +. z *. rect_left r) in
  let y1 = int_of_float (sy +. z *. rect_bottom r) in
  let x2 = int_of_float (sx +. z *. rect_right r) in
  let y2 = int_of_float (sy +. z *. rect_top r) in
  let _ = Graphics.set_color Graphics.blue in
  let _ = Graphics.draw_rect x1 y1 (x2-x1) (y2-y1) in
  let _ = Graphics.set_color Graphics.black in
  match qc with
    | Empty -> ()
    | Leaf (c,d) -> draw_data dparams data_to_string (c,d) Graphics.black
    | Node (nw,ne,se,sw) ->
       let _ = draw_quadtree dparams data_to_string nw in
       let _ = draw_quadtree dparams data_to_string ne in
       let _ = draw_quadtree dparams data_to_string se in
       let _ = draw_quadtree dparams data_to_string sw in
       ()
;;
