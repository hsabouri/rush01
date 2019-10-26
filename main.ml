let (>>=) = Lwt.(>>=)

let pokemon_bg = "

██████
 ████
 █  █
"

let pokemon_fg = "
 █  █



"

let background = ({ bold = None ; underline = None ; blink = None ; reverse = None ; foreground = Some LTerm_style.white ; background = Some LTerm_style.white  }: LTerm_style.t)

let rec loop ui coord =
	LTerm_ui.wait ui >>= function
		| LTerm_event.Key { code = Escape; _ } -> Lwt.return ()
		| LTerm_event.Key { code = q; _ } when q = Char (CamomileLibrary.UChar.of_char 'q') -> Lwt.return ()
		| _ -> loop ui coord

let draw ui matrix (coord: LTerm_geom.coord) =
	let size = LTerm_ui.size ui in
	let ctx = LTerm_draw.context matrix size in
	LTerm_draw.fill_style ctx background
	; LTerm_draw.draw_frame_labelled ctx { row1 = 0; col1 = 0; row2 = size.rows; col2 = size.cols } ~alignment:H_align_center (Zed_string.unsafe_of_utf8 " Pikachu Tamagochi ") LTerm_draw.Light;

	let ctx = LTerm_draw.sub ctx { row1 = 1; col1 = 1; row2 = size.rows - 1; col2 = size.cols - 1 } in

	LTerm_draw.draw_styled_aligned ctx coord.row H_align_center (LTerm_text.eval [
		LTerm_text.B_fg LTerm_style.yellow ;
		LTerm_text.S pokemon_bg ;
		LTerm_text.E_fg
	])

	; LTerm_draw.draw_styled_aligned ctx coord.row H_align_center (LTerm_text.eval [
		LTerm_text.B_fg LTerm_style.black ;
		LTerm_text.S pokemon_fg ;
		LTerm_text.E_fg
	])

let main () =
	Lazy.force LTerm.stdout
	>>= fun term ->

	let coord = ref ({ row = 0; col = 100 }: LTerm_geom.coord) in

	LTerm_ui.create term (fun matrix size -> draw matrix size !coord)
		>>= fun ui ->
			Lwt.finalize (fun () -> loop ui coord) (fun () -> LTerm_ui.quit ui)

let () = Lwt_main.run (main ())

(*


let pokemon_string_test = LTerm_text.eval [
	S(pokemon_string);
	B_fg(LTerm_style.rgb 228 0 102);
	S"foreground";
	E_fg;
	S" ";
	B_bg(LTerm_style.rgb 52 89 149);
	S"background";
	E_bg
]

let main () =
	let waiter, wakener = Lwt.wait () in

	let vbox = new LTerm_widget.vbox in

	let pika = new LTerm_widget.label pokemon_string in
	vbox#add pika

	; let hbox = new LTerm_widget.hbox in

	let button = new LTerm_widget.button
		~brackets:("[ ", " ]")
		"exit"
	in
	button#on_click (Lwt.wakeup wakener)

	; hbox#add button

	; vbox#add hbox

	; let frame = new LTerm_widget.frame in
	frame#set vbox
	; frame#set_label ~alignment:LTerm_geom.H_align_center " personnal_space.com "
	; Lazy.force LTerm.stdout >>= fun term ->
		LTerm.enable_mouse term >>= fun () ->
			Lwt.finalize
				(fun () -> LTerm_widget.run term frame waiter)
				(fun () -> LTerm.disable_mouse term)
(*
let main () =
  let waiter, wakener = Lwt.wait () in

  let vbox = new LTerm_widget.vbox in
  let button = new LTerm_widget.button
    ~brackets:("[ ", " ]")
    "exit"
  in
  let label = new LTerm_widget.label "_" in
  button#on_click (Lwt.wakeup wakener);
  vbox#add button;
  vbox#add label;

  let button = new LTerm_widget.button

  for i = 0 to 2 do
    let hbox = new LTerm_widget.hbox in
    let button i =
      let button = new LTerm_widget.button ("button" ^ string_of_int i) in
      button#on_click (fun () -> label#set_text (string_of_int i));
      button
    in
    hbox#add (button (i * 3 + 1));
    hbox#add ~expand:false (new LTerm_widget.vline);
    hbox#add (button (i * 3 + 2));
    hbox#add ~expand:false (new LTerm_widget.vline);
    hbox#add (button (i * 3 + 3));
    vbox#add ~expand:false (new LTerm_widget.hline);
    vbox#add hbox
  done;

  let frame = new LTerm_widget.frame in
  frame#set vbox;
  frame#set_label ~alignment:LTerm_geom.H_align_center " Button test ";

  Lazy.force LTerm.stdout >>= fun term ->
  LTerm.enable_mouse term >>= fun () ->
  Lwt.finalize
    (fun () -> LTerm_widget.run term frame waiter)
	(fun () -> LTerm.disable_mouse term)
*)

let () = Lwt_main.run (main ())
*)