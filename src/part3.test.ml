#use "part3.ml" ;;
     
     
(*
 * Question 15
 *)

new_simple_test (0., 0.) (100., 100.) 100 ;;
  
  
(*
 * Question 16
 *)
  
let q = gen_random_quadtree (make_rect (0., 0.) (20., 20.)) 30 ;;
simple_test q (fun str -> str) ;;
bad_simple_test q (fun str -> str) ;;
