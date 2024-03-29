#use "rect.ml"
     
type 'a quadtree =
  | Q of rect * 'a cell
 and 'a cell =
   | Empty
   | Leaf of coord * 'a
   | Node of 'a quadtree * 'a quadtree * 'a quadtree * 'a quadtree
;;


(* Question 8 *)

let boundary = fun q ->
  let Q(r, _) = q in r
;;


(* Question 9 *)

let rec cardinal = fun q ->
  let Q(_, c) = q in 
  match c with
  | Empty -> 0
  | Leaf _ -> 1
  | Node (nw, ne, se, sw) ->
     cardinal nw + cardinal ne + cardinal se + cardinal sw
;;


(* Question 10 *)

let rec list_of_quadtree = fun q ->
  let Q(_, cell) = q in 
  match cell with
  | Empty -> []
  | Leaf (c, obj) -> (c, obj)::[]
  | Node (nw, ne, se, sw) ->
     (list_of_quadtree nw)@
       (list_of_quadtree ne)@
	 (list_of_quadtree se)@
	   (list_of_quadtree sw)
;;
  

(* Question 11 *)

let split_leaf = fun q c obj ->
  let Q(r, cell) = q in 
  let r1, r2, r3, r4 = rect_split r in
  let newcell =
    match rect_mem r1 c, rect_mem r2 c, rect_mem r3 c, rect_mem r4 c with
    | true, false, false, false -> Node(
				       Q(r1, Leaf(c, obj)),
				       Q(r2, Empty),
				       Q(r3, Empty),
				       Q(r4, Empty))
    | false, true, false, false -> Node(
				       Q(r1, Empty),
				       Q(r2, Leaf(c, obj)),
				       Q(r3, Empty),
				       Q(r4, Empty))
    | false, false, true, false -> Node(
				       Q(r1, Empty),
				       Q(r2, Empty),
				       Q(r3, Leaf(c, obj)),
				       Q(r4, Empty))
    | false, false, false, true -> Node(
				       Q(r1, Empty),
				       Q(r2, Empty),
				       Q(r3, Empty),
				       Q(r4, Leaf(c, obj)))
    | _, _, _, _ -> failwith "split_leaf error"
  in Q(r, newcell)
;;
  
let rec insert = fun q c add_obj ->
  let Q(r, cell) = q in
  if not (rect_mem r c)
  then q
  else match cell with
       | Empty -> Q(r, Leaf (c, add_obj))
       | Leaf (c', obj) ->
	  if coord_equals c c'
	  then failwith "An object already exists at these coordinates"
	  else insert (split_leaf q c' obj) c add_obj
       | Node (nw, ne, se, sw) ->
	  Q(r, Node(
		   insert nw c add_obj,
		   insert ne c add_obj,
		   insert se c add_obj,
		   insert sw c add_obj))
;;
  


(* Question 12 *)
  
let quadtree_of_list = fun l r ->
  let insert_function = fun q item ->
    let c, obj = item in
    insert q c obj
  in
  List.fold_left insert_function (Q(r, Empty)) l
;;


(* Question 13 *)

let rec clean_qt = fun q ->
  let Q(r, cell) = q in
  match cell with 
  | Empty -> Q(r, cell)
  | Leaf _ -> Q(r, cell)
  | Node(Q(r1, c1), Q(r2, c2), Q(r3, c3), Q(r4, c4)) ->
     match c1, c2, c3, c4  with
     (* All sub quadtrees empty *)
     | Empty , Empty , Empty , Empty  -> Q(r, Empty)
					  
     (* Only 1 leaf *)
     | Leaf _, Empty , Empty , Empty  -> Q(r, c1)
     | Empty , Leaf _, Empty , Empty  -> Q(r, c2)
     | Empty , Empty , Leaf _, Empty  -> Q(r, c3)
     | Empty , Empty , Empty , Leaf _ -> Q(r, c4)
					  
     (* Other cases *)
     | _, _, _, _ -> Q(r, Node(
			      clean_qt (Q(r1, c1)),
			      clean_qt (Q(r2, c2)),
			      clean_qt (Q(r3, c3)),
			      clean_qt (Q(r4, c4))))
;;


let rec remove = fun q c ->
  let Q(r, cell) = q in
  clean_qt (Q(r, match cell with
		 | Empty -> cell
		 | Leaf (c1, _) ->
		    if coord_equals c c1
		    then Empty else cell
		 | Node (nw, ne, se, sw) ->
		    Node(
			(remove nw c),
			(remove ne c),
			(remove se c),
			(remove sw c))))
;;
  

(* Alternative, less efficient remove function *)

let rec list_remove = fun q c ->
  let Q(r, _) = q in
  let l = list_of_quadtree q in
  let check_function = fun l item ->
    let item_coord, _ = item in
    if coord_equals item_coord c
    then l
    else item::l
  in
  let l' = List.fold_left check_function [] l in
  quadtree_of_list l' r
;;
