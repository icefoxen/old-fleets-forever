open Vector2d

type gamestate = {
   continue : bool;
   input : gamestate Input.inputContext;
   res : Resources.resourcePool;
   zoom : float;
   loc : vector2D;
   frames : int;
   features : Objs.feature list;
   level : Level.level;
}

let initInput res =
   let ic = Input.createContext () in
   let ic = Input.bindQuit ic (fun g -> {g with continue = false}) in
   let ic = Input.bindMany Input.bindButtonPress ic
      [(Sdlkey.KEY_q, (fun _ g -> {g with continue = false}));
       (Sdlkey.KEY_ESCAPE, (fun _ g -> {g with continue = false}))] in
   ic

let initGamestate ic res = { 
   continue = true;
   input = ic; 
   res = res; 
   loc = (0.,0.); 
   zoom = 1.;
   frames = 0;
   features = [new Objs.rock (0.0, 0.0) (0.0001, 0.0001)];
   level = {Level.background = ignore};
}



let doInput g =
   let ic, g = Input.doInput g.input g in
   {g with input = ic}


let setView g =
   let x, y = g.loc in
   (* 4:3 aspect ratio *)
   let x1 = x +. (1.25 *. g.zoom)
   and x2 = x -. (1.25 *. g.zoom)
   and y1 = y +. g.zoom
   and y2 = y -. g.zoom in
   Drawing.setView x2 x1 y1 y2


let doDrawing g =
   let scene = List.concat (List.map (fun f -> f#draw f 0) g.features) in
   Drawing.drawScene scene

let calc g =
   {g with features = List.map (fun f -> f#calc) g.features}

let rec doMainLoop g =
   if g.continue then begin
      let g = {g with frames = g.frames + 1} in
      let g = doInput g in
      let g = calc g in
      setView g;
      doDrawing g;
      doMainLoop g;
   end
   else
      let frames = float_of_int g.frames
      and now = (float_of_int (Sdltimer.get_ticks ())) /. 1000. in
      Printf.printf "FPS: %f\n" (frames /. now)



let main () =
   let res = Resources.createResourcePool () in

   let mainConf = Resources.getConfig res "cfg/main.cfg" in
   let x = Cfg.getInt mainConf "screen.x"
   and y = Cfg.getInt mainConf "screen.y" in
   Util.init x y;
   
   let ic = initInput res in 
   let g = initGamestate ic res in

   doMainLoop g; 

   Util.quit ()

let _ = main ()
