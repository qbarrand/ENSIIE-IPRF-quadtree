#use "part1.ml"
     
type 'a quadtree =
  | Q of rect * 'a cell
 and 'a cell =
   | Empty
   | Leaf of coord * 'a
   | Node of 'a quadtree * 'a quadtree * 'a quadtree * 'a quadtree
;;
  

(* Question 7 *) 

(* cf. Report *)


(* Question 8 *)

let boundary =
  fun q ->
  match q with
  | Q(r, _) -> r
;;


(* Question 9 *)

let rec cardinal =
  fun q -> 
  match q with
  | Q(_, c) -> match c with
	       | Empty -> 0
	       | Leaf _ -> 1
	       | Node (q1, q2, q3, q4) -> cardinal q1 + cardinal q2 + cardinal q3 + cardinal q4
;;


(* Question 10 *)

let rec list_of_quadtree =
  fun q -> 
  match q with
  | Q(_, cell) -> match cell with
		  | Empty -> []
		  | Leaf (c, obj) -> (c, obj)::[]
		  | Node (q1, q2, q3, q4) ->
		     (list_of_quadtree q1)@
		       (list_of_quadtree q2)@
			 (list_of_quadtree q3)@
			   (list_of_quadtree q4)
;;


(* Question 11 *)

let split_leaf =
  fun q ->
  fun c ->
  fun obj ->
  match q with
  | Q(r, cell) ->
     let r1, r2, r3, r4 = rect_split r in
     let newcell =
       if rect_mem r1 c then Node(Q(r1, Leaf(c, obj)), Q(r2, Empty), Q(r3, Empty), Q(r4, Empty)) else
	 if rect_mem r2 c then Node(Q(r1, Empty), Q(r2, Leaf(c, obj)), Q(r3, Empty), Q(r4, Empty)) else
	   if rect_mem r3 c then Node(Q(r1, Empty), Q(r2, Empty), Q(r3, Leaf(c, obj)), Q(r4, Empty)) else
	     if rect_mem r4 c then Node(Q(r1, Empty), Q(r2, Empty), Q(r3, Empty), Q(r4, Leaf(c, obj))) else
	       failwith "No sub quadtree of which point is member"
     in Q(r, newcell)
;;
  

let rec insert =
  fun q ->
  fun c ->
  fun add_obj ->
  match q with
    Q(r, cell) ->
    if rect_mem r c = false
    then q (*failwith "Object coordinates not within the quadtree's rectangle" *)
    else
      match cell with
      | Empty -> Q(r, Leaf (c, add_obj))
      | Leaf (c', obj) -> insert (split_leaf q c' obj) c add_obj
      | Node (q1, q2, q3, q4) -> Q(r, Node(insert q1 c add_obj, insert q2 c add_obj, insert q3 c add_obj, insert q4 c add_obj))
;;
  


(* Question 12 *)
  
let quadtree_of_list =
  fun l ->
  fun r ->
  List.fold_left (fun q item -> match item with c, obj -> insert q c obj) (Q(r, Empty)) l
;;


(* Question 13 *)

let rec clean_qt =
  fun q ->
  match q with
  | Q(r, cell) ->
     match cell with 
     | Empty -> Q(r, cell)
     | Leaf _ -> Q(r, cell)
     | Node(Q(r1, c1), Q(r2, c2), Q(r3, c3), Q(r4, c4)) ->
	match c1, c2, c3, c4  with
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

let rec remove =
  fun q ->
  fun c ->
  match q with 
  | Q(r, cell) -> let newcell = match cell with
		    | Empty -> cell
		    | Leaf (c1, _) ->
		       if (fst c1) = (fst c) &&
			    (snd c1) = (snd c)
		       then Empty else cell
		    | Node (q1, q2, q3, q4) ->
		       Node(
			   (remove q1 c),
			   (remove q2 c),
			   (remove q3 c),
			   (remove q4 c))
		  in clean_qt (Q(r, newcell))
;;
