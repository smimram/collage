include Stdlib

(** Tiles. *)
module Tile = struct
  type img =
    { img : Rgb24.t; width : int; height : int }

  (** A tile. *)
  type t =
    | H of t list (** horizontal list *)
    | V of t list (** vertical list *)
    | C of t (** center *)
    | P of t (** prioritize *)
    | I of img (** image *)

  (** String representation. *)
  let rec to_string t =
    let rec to_string pa t =
      let p s = if pa then "(" ^ s ^ ")" else s in
      match t with
      | H l -> p (String.concat "|" (List.map (to_string true) l))
      | V l -> p (String.concat "-" (List.map (to_string true) l))
      | C t -> "[" ^ to_string false t ^ "]"
      | P t -> "!" ^ to_string true t
      | I i -> p (Printf.sprintf "%dx%d" i.width i.height)
    in
    to_string false t

  (** Create from string representation. *)
  let parse s =
    Printf.printf "parsing: %s\n%!" s;
    let n = ref 0 in
    let q = Stack.create () in
    let i = ref 0 in
    while !i < String.length s do
      (* Printf.printf "queue: %d\n%!" (Stack.length q); *)
      let arity d =
        if !i+1 >= String.length s || not (Char.is_figure s.[!i+1]) then d
        else
          let n = Char.int_of_figure s.[!i+1] in
          incr i;
          n
      in
      let k =
        (* Printf.printf "%c\n%!" s.[!i]; *)
        match s.[!i] with
        | 'h' ->
           let a = arity 2 in
           let l = List.init (fun _ -> Stack.pop q) a in
           (fun img ->
             let l = List.map (fun t -> t img) l in
             H l
           )
        | 'v' ->
           let a = arity 2 in
           let l = List.init (fun _ -> Stack.pop q) a in
           (fun img ->
             let l = List.map (fun t -> t img) l in
             V l
           )
        | 'c' ->
           let t = Stack.pop q in
           (fun img -> C (t img))
        | 'p' ->
           let t = Stack.pop q in
           (fun img -> P (t img))
        | '_' ->
           let nn = !n in
           incr n;
           (fun img -> img.(nn))
        | _ -> assert false
      in
      Stack.push k q;
      incr i
    done;
    Stack.pop q, !n

  (** Centered tile. *)
  let center t = C t

  (** Tile of image. *)
  let image ?(expand=1) f =
    let img = Images.load f [] in
    let img = match img with Images.Rgb24 img -> img | _ -> assert false in
    let width = expand * img.Rgb24.width in
    let height = expand * img.Rgb24.height in
    I { img; width; height }

  let is_image = function
    | I _ -> true
    | _ -> false

  let is_prioritary = function
    | P _ -> true
    | _ -> false

  (** Minimal width. *)
  let rec width ?(s=0) t =
    let width t = width ~s t in
    match t with
    | H l -> List.fold_left (+) 0 (List.map width l) + (List.length l - 1) * s
    | V l -> List.fold_left max 0 (List.map width l)
    | C t -> width t
    | P t -> width t
    | I i -> i.width

  (** Minimal height. *)
  let rec height ?(s=0) t =
    let height t = height ~s t in
    match t with
    | H l -> List.fold_left max 0 (List.map height l)
    | V l -> List.fold_left (+) 0 (List.map height l) + (List.length l - 1) * s
    | C t -> height t
    | P t -> height t
    | I i -> i.height

  (** Constrain to a box. *)
  let rec constrain ?(s=0) t w h =
    assert (w>0);
    assert (h>0);
    let constrain t w h = constrain ~s t w h in
    let width ?(s=s) t = width ~s t in
    let height ?(s=s) t = height ~s t in
    let ww = width t in
    let hh = height t in
    match t with
    (* TODO: generic handling of prioritary tiles *)
    | H [t1; t2] when is_prioritary t1 ->
       let t1 = constrain t1 w h in
       let t2 = constrain t2 (w - width t1 - s) h in
       H [t1; t2]
    | H [t1; t2] when is_prioritary t2 ->
       let t2 = constrain t2 w h in
       let t1 = constrain t1 (w - width t2 - s) h in
       H [t1; t2]
    | H l ->
       let ss = width t - width ~s:0 t in
       H (List.map (fun t -> constrain t (width t * (w-ss) / (ww-ss)) h) l)
    | V [t1; t2] when is_prioritary t1 ->
       let t1 = constrain t1 w h in
       let t2 = constrain t2 w (h - height t1 - s) in
       V [t1; t2]
    | V [t1; t2] when is_prioritary t2 ->
       let t2 = constrain t2 w h in
       let t1 = constrain t1 w (h - height t2 - s) in
       V [t1; t2]
    | V l ->
       let ss = height t - height ~s:0 t in
       V (List.map (fun t -> constrain t w (height t * (h-ss) / (hh-ss))) l)
    | C t -> C (constrain t w h)
    | P t -> P (constrain t w h)
    | I i ->
       let width, height = i.width, i.height in
       let width, height =
         if width > w then
           (
             w,
             height * w / width
           )
         else width, height
       in
       let width, height =
         if height > h then
         (
           width * h / height,
           h
         )
         else width, height
       in
       I { img = i.img; width; height }

  (** Render a tile. *)
  let rec render ?(s=0) t out x y w h =
    Printf.printf "render %s : +(%d,%d)@(%d,%d)\n%!" (to_string t) x y w h;
    let width ?(s=s) = width ~s in
    let height ?(s=s) = height ~s in
    let render = render ~s in
    assert (width t <= w);
    assert (height t <= h);
    match t with
    | H [t] -> render t out x y w h
    | H l ->
       let y = y + (h - height t) / 2 in
       let s = (w - width ~s:0 t) / (List.length l - 1) in
       let _ =
         List.fold_left
           (fun x t ->
             let w = width t in
             render t out x y w h;
             x + w + s
           ) x l
       in
       ()
    | V [t] -> render t out x y w h
    | V l ->
       let x = x + (w - width t) / 2 in
       let s = (h - height ~s:0 t) / (List.length l - 1) in
       let _ =
         List.fold_left
           (fun y t ->
             let h = height t in
             render t out x y w h;
             y + h + s
           ) y l
       in
       ()
    | C t ->
       let x = x + (w - width t) / 2 in
       let y = y + (h - height t) / 2 in
       let w = width t in
       let h = height t in
       render t out x y w h
    | P t -> render t out x y w h
    | I i ->
       let x = x + (w - width t) / 2 in
       let y = y + (h - height t) / 2 in
       let w = width t in
       let h = height t in
       let img = Rgb24.resize None i.img w h in
       Rgb24.blit img 0 0 out x y w h
end

let white = { Color.r = 255; g = 255; b = 255 }

module Output = struct
  let name = ref "page"

  (* Scaling factor: faster to test... *)
  let scale = ref 1
  let width () = 7016 / !scale
  let height () = 4960 / !scale
  let border () = 200 / !scale
  let separator () = 40 / !scale

  (** Maximum expansion of images. *)
  let expand = ref 2
                           
  let make ?(color=white) () =
    Rgb24.make (width ()) (height ()) color

  let display = ref false
end

let process f =
  let ic = open_in f in
  let page = ref 0 in
  try
    while true do
      try
        let s = input_line ic in
        if s = "" then raise Exit;
        if s.[0] <> '=' then raise Exit;
        Printf.printf "***** PAGE %d *****\n%!" !page;
        let s = String.sub s 1 (String.length s - 1) in
        let s =
          match s with
          | "22" | "2x2" | "2+2" -> "__h__hv"
          | "3x2" -> "___h3___h3v"
          | "33" | "3x3" -> "___h3___h3___h3v3"
          | "1+2" -> "_p__vh"
          | "2+1" -> "__v_ph"
          | "3+1" -> "___v3_ph"
          | _ -> s
        in
        let t,n = Tile.parse s in
        let img =
          Array.init
            n
            (fun _ ->
              let rec f () = let s = input_line ic in if s <> "" && s.[0] <> '#' then s else f () in
              let f = f () in
              Printf.printf "file: %s\n%!" f;
              Tile.image ~expand:!Output.expand f
            )
        in
        let t = t img in
        (* TODO: think harder about where tiles should be centered... *)
        let t = match t with Tile.V _ | Tile.H _ -> Tile.center t | t -> t in
        Printf.printf "processing tile: %s\n%!" (Tile.to_string t);
        let out = Output.make () in
        let b = Output.border () in
        let width = Output.width () - 2*b in
        let height = Output.height () - 2*b in
        let s = Output.separator () in
        let t = Tile.constrain ~s t width height in
        Tile.render ~s t out b b width height;
        let out = Images.Rgb24 out in
        Images.save (Printf.sprintf "%s%03d.jpg" !Output.name !page) (Some Images.Jpeg) [Images.Save_Quality 95] out;
        if !Output.display then
          (
            Graphics.open_graph "";
            Graphics.resize_window (Output.width ()) (Output.height ());
            Graphic_image.draw_image out 0 0;
            ignore (Graphics.read_key ())
          );
        incr page
      with
      | Exit -> ()
    done
  with
  | End_of_file -> close_in ic

let () =
  let file = ref "collage" in
  Arg.parse
    [
      "--display", Arg.Set Output.display, " Display output.";
      "--draft", Arg.Unit (fun () -> Output.scale := 8), " Draft rendering.";
      "--name", Arg.Set_string Output.name, " Output file name.";
    ]
    (fun s -> file := s)
    "collage [options]";
  process !file
