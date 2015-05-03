#use "part5.ml"

(* Question 23 *)
  
let q = gen_random_quadtree (make_rect (0., 0.) (30., 30.)) 30 ;;
simulation_move q (fun str -> str) ;; 


(* Question 23 bonus *)

new_simulation_move q (fun str -> str) ;;  
