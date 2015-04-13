#use "rect.ml"

type 'a quadtree =
  | Q of rect * 'a cell
 and 'a cell =
   | Empty
   | Leaf of coord * 'a
   | Node of 'a quadtree * 'a quadtree * 'a quadtree * 'a quadtree
;;


(* Question 8 *)

let boundary =
  fun q ->
  match q with
  | Q(r, _) -> r
;;


(* Question 9 *)

let rec cardinal =
  fun q -> 
  match q with Q(_, c) ->
	       match c with
	       | Empty -> 0
	       | Leaf _ -> 1
	       | Node (nw, ne, se, sw) ->
		  cardinal nw +
		    cardinal ne +
		    cardinal se +
		    cardinal sw
;;


(* Question 10 *)

let rec list_of_quadtree =
  fun q -> 
  match q with Q(_, cell) ->
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

let split_leaf =
  fun q ->
  fun c ->
  fun obj ->
  match q with Q(r, cell) ->
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

let rec insert =
  fun q ->
  fun c ->
  fun add_obj ->
  match q with Q(r, cell) ->
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
  
let quadtree_of_list =
  fun l ->
  fun r ->
  List.fold_left (fun q item -> match item with c, obj -> insert q c obj) (Q(r, Empty)) l
;;


(* Question 13 *)

let rec clean_qt =
  fun q ->
  match q with Q(r, cell) ->
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


let rec remove =
  fun q ->
  fun c ->
  match q with Q(r, cell) ->
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
  
