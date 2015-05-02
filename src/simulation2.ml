(* draw_trail_with_collisions: float * float * float -> ('a -> string)
 *                             -> 'a quadtree -> coord * float -> coord -> bool
 *)
let draw_trail_with_collisions =  fun dparams f qt ((x0,y0), r) (x1,y1) ->
    let (sx,sy,z) = dparams in
    let a = y1 -. y0 in
    let b = x0 -. x1 in
    let alpha = 2.0 *. atan (a /. (sqrt (a ** 2.0 +. b ** 2.0) -. b)) in
    let l = collision_trail qt (x0,y0) (x1,y1) r in
    let _ = Graphics.set_color Graphics.magenta in
    let x0r = int_of_float (sx +. z *. (x0 -. r *. sin alpha) ) in
    let y0r = int_of_float (sy +. z *. (y0 +. r *. cos alpha) ) in
    let x1r = int_of_float (sx +. z *. (x1 -. r *. sin alpha) ) in
    let y1r = int_of_float (sy +. z *. (y1 +. r *. cos alpha) ) in
    let x0r' = int_of_float (sx +. z *. (x0 +. r *. sin alpha) ) in
    let y0r' = int_of_float (sy +. z *. (y0 -. r *. cos alpha) ) in
    let x1r' = int_of_float (sx +. z *. (x1 +. r *. sin alpha) ) in
    let y1r' = int_of_float (sy +. z *. (y1 -. r *. cos alpha) ) in
    let _ = Graphics.moveto x0r y0r in
    let _ = Graphics.lineto x1r y1r in
    let _ = Graphics.lineto x1r' y1r' in
    let _ = Graphics.lineto x0r' y0r' in
    let _ = Graphics.lineto x0r y0r in
    let _ = List.map (fun data -> draw_data dparams f data Graphics.cyan) l in
    l = []
;;


(* simulation_move: 'a quadtree -> ('a -> string) -> unit
 *)
let simulation_move = fun qt f ->
    let dparams = init (boundary qt) in
    let _ = draw_quadtree dparams f qt in
    let disk = get_disk dparams in
    if draw_disk_with_collisions dparams f qt disk
    then
        let (xd,yd) = get_point dparams in
        let r = snd disk in
        let _ = Graphics.clear_graph () in
        let _ = draw_disk dparams disk Graphics.green false in
        let _ = draw_quadtree dparams f qt in
        let _ = draw_trail_with_collisions dparams f qt disk (xd,yd) in
        let _ = draw_disk_with_collisions dparams f qt ((xd,yd),r) in
        wait_and_quit ()
    else
        wait_and_quit ()
;;

