let (>>=) = Lwt.(>>=)

let main () =
	let waiter, wakener = Lwt.wait () in

	let vbox = new LTerm_widget.vbox in

	let pika = new LTerm_widget.label Render.pika_tired in
	vbox#add pika
	; let spacer = new LTerm_widget.spacing ~rows:10 () in
	vbox#add spacer

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
