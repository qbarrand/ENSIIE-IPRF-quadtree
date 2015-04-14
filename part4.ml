#use "part3.ml"
#use "simulation1.ml"

let collision_disk_point = fun ((c: coord), (r: float)) (p: coord) ->
  ((fst p) -. (fst c)) ** 2. +. ((snd p) -. (snd c)) ** 2. <= r ** 2.
;;


(* Question 18 *)

let rec clip = fun q r ->
  let Q(rq, cq) = q in
  if not (rect_intersect rq r)
  then Q(rq, Empty)
  else match cq with
       | Empty -> q
       | Leaf (c', e) ->
	  if rect_mem r c' then q else Q(rq, Empty)
       | Node(nw, ne, se, sw) ->
	  clean_qt (Q(r, Node(
			     clip nw rq,
			     clip ne rq,
			     clip se rq,
			     clip sw rq)))
;;


(* Question 19 *)
  
let rec collision_disk = fun q ((c: coord), (r: float)) ->
  match q with
  | Q(qr, qc) ->
     match qc with
     | Empty -> []
     | Leaf(c', o) ->
	if collision_disk_point (c, r) c'
	then [(c', o)]
	else []
     | Node(q1, q2, q3, q4) ->
	(collision_disk q1 (c, r))@
	  (collision_disk q2 (c, r))@
	    (collision_disk q3 (c, r))@
	      (collision_disk q4 (c, r))
;;


(* Question 20 *)

let r0 = make_rect (0., 0.) (2., 2.) ;;

let c0 = 0.3, 0.3 ;;
let c1 = 0.7, 0.7 ;;
let c2 = 1.2, 1.2 ;;
let c3 = 1.6, 1.6 ;;
    
let q0 = Q(r0, Leaf(c0, "obj0")) ;;

let q1 = insert q0 c1 "obj1" ;;
let q2 = insert q1 c2 "obj2" ;;
let q3 = insert q2 c3 "obj3" ;;

simulation_placement q3 (fun str -> str) ;; 
