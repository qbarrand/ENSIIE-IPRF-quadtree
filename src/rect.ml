#use "coord.ml"

type rect = R of coord * coord ;;


(* Question 1 *)
  
let make_rect = fun a b ->
  let c1 = (min (fst a) (fst b), max (snd a) (snd b)) in
  let c2 = (max (fst a) (fst b), min (snd a) (snd b)) in
  R(c1, c2)
;; 
  
  
(* Question 2 *)
  
let rect_left = fun r ->
  let R((x1, _), (_, _)) = r in x1
;;

let rect_right = fun r ->
  let R((_, _), (x2, _)) = r in x2
;;

let rect_bottom = fun r ->
  let R((_, _), (_, y2)) = r in y2
;;

let rect_top = fun r ->
  let R((_, y1), (_, _)) = r in y1
;;


(* Question 3 *)

let rect_length = fun r ->
  rect_right r -. rect_left r
;;

let rect_height = fun r ->
  rect_top r -. rect_bottom r
;;


(* Question 4 *)

let rect_mem = fun r a ->
  fst a >= rect_left r
  && fst a <= rect_right r
  && snd a <= rect_top r
  && snd a >= rect_bottom r
;;


(* Question 5 *)

let rect_intersect = fun r1 r2 ->
  (* Any point of r2 into r1 ? *)
  rect_mem r1 (rect_left r2, rect_top r2)
  || rect_mem r1 (rect_left r2, rect_bottom r2)
  || rect_mem r1 (rect_right r2, rect_top r2)
  || rect_mem r1 (rect_right r2, rect_bottom r2)
  (* Any point of r1 into r2 ? *)
  || rect_mem r2 (rect_left r1, rect_top r1)
  || rect_mem r2 (rect_left r1, rect_bottom r1)
  || rect_mem r2 (rect_right r1, rect_top r1)
  || rect_mem r2 (rect_right r1, rect_bottom r1)
;;


(* Question 6 *)

let rect_split = fun r ->
  let p = (rect_left r) +. ((rect_length r) /. 2.),
	  (rect_bottom r +. (rect_height r) /. 2.)
  in
  make_rect (rect_left r, rect_top r) p,
  make_rect p (rect_right r, rect_top r),
  make_rect (rect_left r, rect_bottom r) p,
  make_rect p (rect_right r, rect_bottom r)
;;


(* Other functions *)

let rect_tostring = fun r ->
  let R(c1, c2) = r in
  Printf.sprintf "Rect(%s, %s)" (coord_tostring c1) (coord_tostring c2)
;;
