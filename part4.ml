#use "part3.ml"

let collision_disk_point = fun ((c: coord), (r: float)) (p: coord) ->
  ((fst p) -. (fst c)) ** 2. +. ((snd p) -. (snd c)) ** 2. <= r ** 2.
;;


(* Question 18 *)

let rec clip = fun q r ->
  let Q(rq, cq) = q in
  if not (rect_intersect r rq)
  then Q(rq, Empty)
  else match cq with
       | Empty -> q
       | Leaf (c', _) -> if rect_mem r c'
			 then q
			 else Q(rq, Empty)
       | Node(nw, ne, se, sw) ->
	  clean_qt (
	      Q(r, Node(clip nw r, clip ne r, clip se r, clip sw r)))
;;


(* Question 19 *)
  
let rec collision_disk = fun q ((c: coord), (r: float)) ->
  (* Clip the QuadTree *)
  let (cx, cy) = c in
  let q' = clip q (make_rect (cx -. r, cy +. r) (cx +. r, cy -. r)) in
  let Q(_, qc') = q' in
  match qc' with
  | Empty -> []
  | Leaf(c', o) ->
     if collision_disk_point (c, r) c'
     then [(c', o)]
     else []
  | Node(q1, q2, q3, q4) ->
    (collision_disk q1 (c, r))
    @(collision_disk q2 (c, r))
    @(collision_disk q3 (c, r))
    @(collision_disk q4 (c, r))
;;
