let (>>=) = Lwt.(>>=)

module Pikatchu = Pika.Pika

let main () =
	let pika = ref (Pikatchu.return (100, 100, 100, 100)) in
	let waiter, wakener = Lwt.wait () in

	let vbox = new LTerm_widget.vbox in

	let pika_renderer = new Pikatchu.renderer pika in
	vbox#add pika_renderer

	; vbox#add (new Pikatchu.bar_renderer pika)

	; let hbox = new LTerm_widget.hbox in
	let button = new LTerm_widget.button
		~brackets:("[ ", " ]")
		"EAT"
	in
	button#on_click (fun _ -> ())
	; hbox#add button
	; let button = new LTerm_widget.button
		~brackets:("[ ", " ]")
		"THUNDER"
	in
	button#on_click (fun _ -> ())
	; hbox#add button
	; let button = new LTerm_widget.button
		~brackets:("[ ", " ]")
		"BATH"
	in
	button#on_click (fun _ -> ())
	; hbox#add button
	; let button = new LTerm_widget.button
		~brackets:("[ ", " ]")
		"KILL"
	in
	button#on_click (fun _ -> ())

	; hbox#add button
	; vbox#add hbox

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

let () = Lwt_main.run (main ())
