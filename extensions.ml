let c_equals =
  fun (c1: float * float) ->
  fun (c2: float * float) ->
  fst c1 = fst c2 && snd c1 = snd c2
;;

let c_tostring =
  fun (c: float*float) ->
  Printf.printf "Point(%f, %f)\n" (fst c) (snd c)
;;
