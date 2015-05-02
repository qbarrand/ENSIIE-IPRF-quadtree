#use "part4.ml"
#use "simulation1.ml"

(* Question 18 bonus *)
  
let graphical_clip = fun q f ->
  let r = boundary q in
  let dparams = init r in
  let sx,sy,z = dparams in

  (* Draw initial QuadTree *)
  let _ = draw_quadtree dparams f q in
  
  (* Get 2 points *)
  let c1 = get_point dparams in
  let c2 = get_point dparams in
  let r' = make_rect c1 c2 in

  (* Debug *)
  let _ = Printf.printf "%s\n" (rect_tostring r') in
  
  (* Draw red rectangle and wait for key press *)
  let x1 = int_of_float (sx +. z *. rect_left r') in
  let y1 = int_of_float (sy +. z *. rect_bottom r') in
  let x2 = int_of_float (sx +. z *. rect_right r') in
  let y2 = int_of_float (sy +. z *. rect_top r') in
  let _ = Graphics.set_color Graphics.red in
  let _ = Graphics.draw_rect x1 y1 (x2-x1) (y2-y1) in
  let _ = Graphics.set_color Graphics.black in  
  let _ = Graphics.wait_next_event [ Graphics.Key_pressed ] in

  (* Clip the QuadTree and draw it *)
  let _ = Graphics.close_graph in 
  let q' = clip q r' in

  let dparams = init r' in
  let _ = draw_quadtree dparams f q' in
  let _ = wait_and_quit () in
  q'
;;

let q = gen_random_quadtree (make_rect (0., 0.) (30., 30.)) 20 ;;
let qbis = graphical_clip q (fun str -> str) ;;

simple_test qbis (fun str -> str);;
