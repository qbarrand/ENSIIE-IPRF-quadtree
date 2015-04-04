#use "part2.ml"

#use "display.ml"

let r0 = make_rect (0., 0.) (2., 2.) ;;
  
let c0 = 1., 1. ;;

let q0 = Q(r0, Leaf(c0, 0)) ;;
let q1 = Q(r0, Leaf(c0, "obj1")) ;;
let q2 = Q(r0, Leaf((2., 2.), "obj2")) ;;

  (*
simple_test q0 string_of_int ;;
simple_test q1 (fun str -> str) ;;
simple_test q2 (fun str -> str) ;;
   *)
  
let r1, r2, r3, r4 = rect_split r0 ;;
rect_mem r1 (1.5, 1.5) ;;
rect_mem r2 (1.5, 1.5) ;;
rect_mem r3 (1.5, 1.5) ;;
rect_mem r4 (1.5, 1.5) ;;

  
let q3 = insert q1 (1.5, 1.5) "obj3" ;;

simple_test q3 (fun str -> str) ;;
  

(* simple_test: 'a quadtree -> ('a -> string) -> unit
 *)
let new_simple_test = fun qt f ->
    let r = boundary qt in
    let dparams = init r in
    let _ = draw_quadtree dparams f qt in
    wait_and_quit ()
;;
