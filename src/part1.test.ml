#use "part1.ml"

(* Q1 *)

let r1 = make_rect (0., 0.) (1., 1.) ;;
let r2 = make_rect (1., 1.) (3., 3.) ;;


(* Q2 *)

rect_length r1 ;;
rect_length r2 ;;

rect_height r1 ;;
rect_height r2 ;;


(* Q4 *)

rect_mem r2 (1.5, 1.5) ;;
rect_mem r2 (4., 4.) ;;


(* Q5 *)

rect_intersect r1 r2 ;;
rect_intersect r1 (make_rect (0.5, 0.5) (2., 2.)) ;;


(* Q6 *)

rect_split r1 ;;
rect_split r2 ;;
