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

let boundary = fun q ->
  match q with
  | Q(r, _) -> r
;;


(* Question 9 *)

let rec cardinal = fun q -> 
  match q with
  | Q(_, c) -> match c with
    | Empty -> 0
    | Leaf _ -> 1
    | Node (q1, q2, q3, q4) -> cardinal q1 + cardinal q2 + cardinal q3 + cardinal q4
;;


(* Question 10 *)

let rec list_of_quadtree = fun q -> 
  match q with
  | Q(_, cell) -> match cell with
    | Empty -> []
    | Leaf (c, obj) -> (c, obj)::[]
    | Node (q1, q2, q3, q4) -> (list_of_quadtree q1)@(list_of_quadtree q2)@(list_of_quadtree q3)@(list_of_quadtree q4)
;;


(* Question 11 *)

let insert = fun q -> fun c -> fun add_obj -> 
  match q with
  | Q(r, cell) -> let newcell = match cell with
    | Empty -> cell
    | Leaf (c1, _) -> if c1 = c then Leaf(c1, add_obj) else cell
    | Node _ -> cell
                  in Q(r, newcell)
;;


(* Question 12 *)


let quadtree_of_list = fun l -> fun r ->
  match r with
  | R _ -> let newcell = match l with
    | [] -> Empty
    | x::xs -> 
           in Q(r, newcell)
;;



(* Question 13 *)
(* TODO supprimer rectangles inutiles *)

let rec clean_node = fun cell ->
  match cell with
  | Node(q1, q2, q3, q4) -> match q1, q2, q3, q4 with
    | Q(_, Empty), Q(_, Empty), Q(_, Empty), Q(_, Empty) -> Empty
    | Q(r1, c1), Q(r2, c2), Q(r3, c3), Q(r4, c4) -> 
      Node(
        Q(r1, clean_node c1),
        Q(r2, clean_node c2),
        Q(r3, clean_node c3),
        Q(r4, clean_node c4))
;;

let remove = fun q -> fun c ->
  match q with 
  | Q(r, cell) -> let newcell = match cell with
    | Empty -> cell
    | Leaf (c1, _) -> if c1 = c then Empty else cell
    | Node (q1, q2, q3, q4) -> clean (Node (q1, q2, q3, q4))
                  in Q(r, newcell)
;;


