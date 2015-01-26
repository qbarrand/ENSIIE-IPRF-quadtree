#use "part1.ml"

type 'a quadtree =
| Q of rect * 'a cell
and 'a cell =
| Empty
| Leaf of coord * 'a
| Node of 'a quadtree * 'a quadtree * 'a quadtree * 'a quadtree
;;


(* Question 7 *) 

(* TODO *)


(* Question 8 *)

let boundary = fun q -> match q with
  | Q(r, _) -> r
;;


(* Question 9 *)

let rec cardinal = fun q -> match q with
  | Q(_, c) -> match c with
    | Empty -> 0
    | Leaf (_, _) -> 1
    | Node (q1, q2, q3, q4) -> cardinal q1 + cardinal q2 + cardinal q3 + cardinal q4
;;


(* Question 10 *)

let rec list_of_quadtree = fun q -> match q with
  | Q(_, cell) -> match cell with
    | Empty -> []
    | Leaf (c, a) -> (c, a)::[]
    | Node (q1, q2, q3, q4) -> (list_of_quadtree q1)@(list_of_quadtree q2)@(list_of_quadtree q3)@(list_of_quadtree q4)
;;


(* Question 11 *)

(*
let insert = fun q -> c -> a -> match q with
  | Q(r, cell) -> match cell with
    | Empty -> cell
*)
