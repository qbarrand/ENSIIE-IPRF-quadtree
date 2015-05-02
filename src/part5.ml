#use "part4.ml"

(* Question 21 *)

let collision_trail_point = fun (p: coord) (q: coord) (r: float) (m: coord) ->
  let x, y = m in
  let xp, yp = p in
  let xq, yq = q in
  let pm_dot_pq = (x -. xp) *. (xq -. xp) +. (y -. yp) *. (yq -. yp) in
  let qm_dot_qp = (x -. xq) *. (xp -. xq) +. (y -. yq) *. (yp -. yq) in
  let d = abs_float ((yq -. yp) *. x
		     -. (xq -. xp) *. y
		     +. xq *. yp
		     -. yq *. xp)
	  /. sqrt ((xq -. xp) ** 2.
		   +. (yq -. yp) ** 2.)
  in
  collision_disk_point (q, r) m
  || (d <= r && pm_dot_pq >= 0. && qm_dot_qp >= 0.)
;;


(* Question 22 *)

let rec collision_trail = fun q (cs: coord) (cd: coord) (r: float) ->
  let Q(r', c) = q in
  match c with
  | Empty -> []
  | Leaf(c', o) ->
     if collision_trail_point cs cd r c'
     then [(c', o)]
     else []
  | Node(q1, q2, q3, q4) ->
     (collision_trail q1 cs cd r)
     @(collision_trail q2 cs cd r)
     @(collision_trail q3 cs cd r)
     @(collision_trail q4 cs cd r)
;;
  
(* Question 23 *)

#use "display.ml"
#use "simulation1.ml"
#use "simulation2.ml"
  
let q = gen_random_quadtree (make_rect (0., 0.) (30., 30.)) 30 ;;
simulation_move q (fun str -> str) ;; 

(* Bonus *)
  
let rec get_new_destination = fun dparams disk qt f ->
  let (xd,yd) = get_point dparams in
  let r = snd disk in
  let _ = Graphics.clear_graph () in
  let _ = draw_disk dparams disk Graphics.green false in
  let _ = draw_quadtree dparams f qt in
  let b1 = draw_trail_with_collisions dparams f qt disk (xd,yd) in
  let b2 = draw_disk_with_collisions dparams f qt ((xd,yd),r) in
  if b1 && b2
  then
    let new_disk = (xd, yd), r in
    get_new_destination dparams new_disk qt f
  else wait_and_quit ()
;;
  
let new_simulation_move = fun qt f ->
  let dparams = init (boundary qt) in
  let _ = draw_quadtree dparams f qt in
  let disk = get_disk dparams in
  if draw_disk_with_collisions dparams f qt disk
  then get_new_destination dparams disk qt f
  else wait_and_quit ()
;;

let q = gen_random_quadtree (make_rect (0., 0.) (30., 30.)) 30 ;;
new_simulation_move q (fun str -> str) ;; 
