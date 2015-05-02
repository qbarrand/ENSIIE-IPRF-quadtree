#use "part4.ml"

(* Question 21 *)

let collision_trail_point = fun (p: coord) (q: coord) (r: float) (m: coord) ->
  collision_disk_point (p, r) m
  || collision_disk_point (q, r) m
  || let d = (abs_float
		(fst m *. (snd q -. snd p) -.
		   (snd m *. (fst q -. fst p)) +.
		   fst q *. snd p -.
		   snd q *. fst p))
	     /.
	       (sqrt (((fst q) -. (fst p)) ** 2. +. ((snd q) -. snd p) ** 2.))
     in
     d <= r
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
