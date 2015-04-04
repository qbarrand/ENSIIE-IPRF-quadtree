#use "part1.test.ml"
#use "part2.ml"

#use "display.ml"

let r = make_rect (0., 0.) (3., 3.) ;;

let q = Q(r, Empty) ;;

let q0 = insert q (1., 1.) "obj1" ;;

 simple_test q0 (fun str -> str);;
