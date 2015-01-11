type coord = float * float ;;
type rect = coord * coord ;;


(* Question 1 *)

let make_rect = fun a -> fun b ->
  let c1 = (min (fst a) (fst b), max (snd a) (snd b)) in
  let c2 = (max (fst a) (fst b), min (snd a) (snd b)) in
  (c1, c2)
;; 

let r1 = make_rect (0., 0.) (1., 1.) ;;
let r2 = make_rect (1., 1.) (3., 3.) ;;


(* Question 2 *)

let rect_length = fun r ->
  fst (snd r) -. fst (fst r)
;;

let rect_height = fun r ->
  snd (fst r) -. snd (snd r)
;;  

rect_length r1 ;;
rect_length r2 ;;

rect_height r1 ;;
rect_height r2 ;;

(* Question 3 *)

let rect_mem = fun r -> fun a ->
  fst a > fst (fst r) &&
    fst a < fst (snd r) &&
    snd a < snd (fst r) &&
    snd a > snd (snd r)
;;

rect_mem r2 (1.5, 1.5) ;;
rect_mem r2 (4., 4.) ;;


(* Question 4 *)

let rect_split = fun r -> fun p ->
  let sr1 = make_rect (fst r) p in
  let sr2 = make_rect p (fst (snd r), snd (fst r)) in
  let sr3 = make_rect (fst (fst r), snd (snd r)) p in
  let sr4 = make_rect p (snd r) in
  (sr1, sr2, sr3, sr4)
;;

rect_split r1 (0.5, 0.5) ;;
rect_split r2 (2., 2.) ;;
