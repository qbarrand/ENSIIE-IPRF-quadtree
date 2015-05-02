#use "quadtree.ml"
#use "display.ml"

(*
 * General purpose testing for previous questions
 *)

let r0 = make_rect (0., 0.) (2., 2.) ;;

let c0 = 0.3, 0.3 ;;
let c1 = 0.7, 0.7 ;;
let c2 = 1.2, 1.2 ;;
let c3 = 1.6, 1.6 ;;

let q0 = Q(r0, Leaf(c0, "obj0")) ;;

let id = fun str -> str ;;

let q1 = insert q0 c1 "obj1" ;;
let q2 = insert q1 c2 "obj2" ;;
let q3 = insert q2 c3 "obj3" ;;

simple_test q0 id ;;
simple_test q1 id ;;
simple_test q2 id ;;
simple_test q3 id ;;

let q4 = remove q3 c3 ;;
let q5 = remove q2 c2 ;;
let q6 = remove q1 c1 ;;

simple_test q4 id ;;
simple_test q5 id ;;
simple_test q6 id ;;

let q7 = insert q0 (1.5, 1.5) "obj" ;;

(* This should raise an exception as an object already exists
 * at (1.5, 1.5)
 *)
let q8 = insert q7 (1.5, 1.5) "obj" ;;
simple_test q7 id ;;
