(* draw_data: (float * float * float) -> ('a -> string) -> coord * 'a
 *            -> Graphics.color -> unit
 *)
let draw_data = fun (sx,sy,z) data_to_string data col ->
    let ((cx,cy), label) = data in
    let x = int_of_float (sx +. z *. cx) in
    let y = int_of_float (sy +. z *. cy) in
    let _ = Graphics.set_color col in
    let _ = Graphics.draw_circle x y 1 in
    let _ = Graphics.moveto (x+3) (y-2) in
    let _ = Graphics.draw_string (data_to_string label) in
    let _ = Graphics.set_color Graphics.black in
    ()
;;

(* draw_quadtree: float * float * float -> ('a -> string) -> 'a quadtree
 *                -> unit
 *)
let rec draw_quadtree = fun dparams data_to_string qt ->
    let sx,sy,z = dparams in
    let Q (r, qc) = qt in
    let x1 = int_of_float (sx +. z *. rect_left r) in
    let y1 = int_of_float (sy +. z *. rect_bottom r) in
    let x2 = int_of_float (sx +. z *. rect_right r) in
    let y2 = int_of_float (sy +. z *. rect_top r) in
    let _ = Graphics.set_color Graphics.blue in
    let _ = Graphics.draw_rect x1 y1 (x2-x1) (y2-y1) in
    let _ = Graphics.set_color Graphics.black in
    match qc with
    | Empty -> ()
    | Leaf (c,d) -> draw_data dparams data_to_string (c,d) Graphics.black
    | Node (nw,ne,se,sw) ->
            let _ = draw_quadtree dparams data_to_string nw in
            let _ = draw_quadtree dparams data_to_string ne in
            let _ = draw_quadtree dparams data_to_string se in
            let _ = draw_quadtree dparams data_to_string sw in
            ()
;;


(* wait_and_quit: unit -> unit
 *)
let wait_and_quit = fun () ->
    let _ = Graphics.wait_next_event [ Graphics.Key_pressed ] in
    Graphics.close_graph ()
;;

(* init: rect -> float * float * float
 *)
let init = fun r ->
    let _ = Graphics.open_graph "" in
    let _ = Graphics.set_color Graphics.black in
    let m = 10 in
    let w = Graphics.size_x () - 2 * m in
    let h = Graphics.size_y () - 2 * m in
    let sx = (float_of_int m) -. rect_left r in
    let sy = (float_of_int m) -. rect_bottom r in
    let zx = (float_of_int w) /. (rect_right r -. rect_left r) in
    let zy = (float_of_int h) /. (rect_top r -. rect_bottom r) in
    let z = min zx zy in
    (sx, sy, z)
;;


(* simple_test: 'a quadtree -> ('a -> string) -> unit
 *)
let simple_test = fun qt f ->
    let r = boundary qt in
    let dparams = init r in
    let _ = draw_quadtree dparams f qt in
    wait_and_quit ()
;;
