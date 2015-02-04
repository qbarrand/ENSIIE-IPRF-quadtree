#use "part2.ml"

#use "display.ml"

let r0 = make_rect (0., 0.) (1., 1.) ;;
let r1 = make_rect (1., 1.) (2., 2.) ;;
let r1 = make_rect (2., 2.) (3., 3.) ;;


let c1 = 1.5, 1.5 ;;
let obj1 = "obj1" ;;
let obj1bis = 1 ;;

let qt0 = Q(r0, Empty) ;;
let qt1 = Q(r1, Leaf(c1, obj1bis)) ;;

let func  = fun str -> str ;;

simple_test qt0 func ;;
simple_test qt1 string_of_int ;;
