type coord = float * float ;;

let coord_equals = fun (c1: coord) (c2: coord) ->
  fst c1 = fst c2 && snd c1 = snd c2
;;

let coord_tostring = fun (c: coord) ->
  Printf.printf "Point(%f, %f)\n" (fst c) (snd c)
;;
