#use "part4.ml"

(* Question 21 *)

let collision_trail_point =
  fun (p: coord) ->
  fun (q: coord) ->
  fun (r: float) ->
  fun (m: coord) ->
  if not (collision_disk_point (p, r) m) &&
       not (collision_disk_point (q, r) m)
  then let d = (abs_float
		  (fst m *. (snd q -. snd p) -.
		     (snd m *. (fst q -. fst p)) +.
			fst q *. snd p -.
		     snd q *. fst p))
	       /.
		 (sqrt (((fst q) -. (fst p)) ** 2. +. ((snd q) -. snd p) ** 2.))
		  in
		  d <= r
  else true
;;


  (* Question 22 *)

let rec collision_trail =
  fun q ->
  fun (cs: coord) ->
  fun (cd: coord) ->
  fun (r: float) ->
  match q with Q(r', c) ->
    match c with
    | Empty -> []
    | Leaf(c', o) ->
       if collision_trail_point cs cd r c'
       then [(c', o)]
       else []
    | Node(q1, q2, q3, q4) ->
       (collision_trail q1 cs cd r)@
	 (collision_trail q2 cs cd r)@
	   (collision_trail q3 cs cd r)@
	     (collision_trail q4 cs cd r)
;;
	 
       
  (* Question 23 *)

#use "display.ml"
#use "simulation2.ml"
  
let r0 = make_rect (0., 0.) (2., 2.) ;;

let c0 = 0.3, 0.3 ;;
let c1 = 0.7, 0.7 ;;
let c2 = 1.2, 1.2 ;;
let c3 = 1.6, 1.6 ;;
    
let q0 = Q(r0, Leaf(c0, "obj0")) ;;

let q1 = insert q0 c1 "obj1" ;;
let q2 = insert q1 c2 "obj2" ;;
let q3 = insert q2 c3 "obj3" ;;

simulation_move q3 (fun str -> str) ;; 
