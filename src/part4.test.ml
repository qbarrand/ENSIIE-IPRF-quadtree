#use "part4.ml"

(* Question 20 *)

#use "simulation1.ml"
  
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
