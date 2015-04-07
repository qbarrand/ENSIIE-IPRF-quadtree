(* get_point: float * float * float -> coord
 *)
let get_point = fun (sx,sy,z) ->
    let st = Graphics.wait_next_event [ Graphics.Button_down ] in
    let x = (float_of_int st.Graphics.mouse_x -. sx) /. z in
    let y = (float_of_int st.Graphics.mouse_y -. sy) /. z in
    x, y
;;

(* get_disk: float * float * float -> coord * float
 *)
let get_disk = fun (sx,sy,z) ->
    let st = Graphics.wait_next_event [ Graphics.Button_down ] in
    let x = (float_of_int st.Graphics.mouse_x -. sx) /. z in
    let y = (float_of_int st.Graphics.mouse_y -. sy) /. z in
    let st' = Graphics.wait_next_event [ Graphics.Button_up ] in
    let x' = (float_of_int st'.Graphics.mouse_x -. sx) /. z in
    let y' = (float_of_int st'.Graphics.mouse_y -. sy) /. z in
    let r = sqrt ((x-.x')**2.0 +. (y-.y')**2.0) in
    (x, y), r
;;


(* draw_disk: float * float * float -> (coord * float) -> Graphics.color
 *            -> bool -> unit
 *)
let draw_disk = fun (sx,sy,z) ((x,y),r) col full ->
    let xr = int_of_float (sx+.z*.x) in
    let yr = int_of_float (sy+.z*.y) in
    let rr = int_of_float (z*.r) in
    let _ = Graphics.set_color col in
    (if full then Graphics.fill_circle else Graphics.draw_circle) xr yr rr
;;


(* draw_disk_with_collisions: float * float * float -> ('a -> string)
 *                             -> 'a quadtree -> coord * float -> bool
 *)
let draw_disk_with_collisions = fun dparams f qt disk ->
    let l = collision_disk qt disk in
    let b, col =
        match l with
        | [] -> true, Graphics.green
        | _ -> false, Graphics.yellow
    in
    let _ = draw_disk dparams disk col true in
    let _ = List.map (fun data -> draw_data dparams f data Graphics.red) l in
    b
;;



(* simulation_placement: 'a quadtree -> ('a -> string) -> unit
 *)
let simulation_placement = fun qt f ->
    let dparams = init (boundary qt) in
    let _ = draw_quadtree dparams f qt in
    let disk = get_disk dparams in
    let _ = draw_disk_with_collisions dparams f qt disk in
    wait_and_quit ()
;;
