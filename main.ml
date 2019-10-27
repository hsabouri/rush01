let (>>=) = Lwt.(>>=)

module Pikatchu = Pika.Pika

let applyAction f pika = pika := f !pika
let launchSave newPath path = path := newPath

let main savePath =
	let stats = match Save.openSave savePath with
		| None -> (100, 100, 100, 100)
		| Some a -> a
	in
	let pika = ref (Pikatchu.return stats) in
	let waiter, wakener = Lwt.wait () in

	let vbox = new LTerm_widget.vbox in

	let pika_bar = new Pikatchu.bar_renderer pika in
	vbox#add pika_bar
	; let pika_renderer = new Pikatchu.renderer pika in
	vbox#add pika_renderer


	; let hbox = new LTerm_widget.hbox in
	let button = new LTerm_widget.button
		~brackets:("[ ", " ]")
		"EAT"
	in
	button#on_click (fun _ -> applyAction Pikatchu.eat pika)
	; hbox#add button
	; let button = new LTerm_widget.button
		~brackets:("[ ", " ]")
		"THUNDER"
	in
	button#on_click (fun _ -> applyAction Pikatchu.thunder pika)
	; hbox#add button
	; let button = new LTerm_widget.button
		~brackets:("[ ", " ]")
		"BATH"
	in
	button#on_click (fun _ -> applyAction Pikatchu.bath pika)
	; hbox#add button
	; let button = new LTerm_widget.button
		~brackets:("[ ", " ]")
		"DANCE"
	in
	button#on_click (fun _ -> applyAction Pikatchu.dance pika)
	; hbox#add button
	; let button = new LTerm_widget.button
		~brackets:("[ ", " ]")
		"KILL"
	in
	button#on_click (fun _ -> applyAction Pikatchu.kill pika)

	; hbox#add button
	; vbox#add hbox

	; let hbox = new LTerm_widget.hbox in
	let button = new LTerm_widget.button
		~brackets:("[ ", " ]")
		"save"
	in
	button#on_click (fun _ -> Save.writeSave (Pikatchu.raw !pika) savePath)
	; hbox#add button
	; let button = new LTerm_widget.button
		~brackets:("[ ", " ]")
		"exit"
	in
	button#on_click (Lwt.wakeup wakener)
	; hbox#add button
	; vbox#add hbox

	; ignore (Lwt_engine.on_timer 1.0 true (fun _ -> applyAction Pikatchu.one_sec pika ; pika_bar#set_text ""))
	; let frame = new LTerm_widget.frame in
	frame#set vbox
	; frame#set_label ~alignment:LTerm_geom.H_align_center " Pikamotchi "
	; Lazy.force LTerm.stdout >>= fun term ->
		LTerm.enable_mouse term >>= fun () ->
			Lwt.finalize
				(fun () -> LTerm_widget.run term frame waiter)
				(fun () -> LTerm.disable_mouse term)

let to_string s = match s with
	| "save/save.itama1" -> "Save 1"
	| "save/save.itama2" -> "Save 2"
	| "save/save.itama3" -> "Save 3"
	| _ -> "Save 4"

let save () =
	let save = ref "save/save.itama1" in
	let waiter, wakener = Lwt.wait () in

	let vbox = new LTerm_widget.vbox in

	 let hbox = new LTerm_widget.hbox in
	let button = new LTerm_widget.button
		~brackets:("[ ", " ]")
		"SAVE 1"
	in
		let savePrint = (new LTerm_widget.label (to_string !save)) in
		 vbox#add savePrint
		; savePrint#set_text (to_string !save)
		; button#on_click (fun _ -> launchSave "save/save.itama1" save; savePrint#set_text (to_string !save))
	; hbox#add button
	; let button = new LTerm_widget.button
		~brackets:("[ ", " ]")
		"SAVE 2"
	in
		button#on_click (fun _ -> launchSave "save/save.itama2" save; savePrint#set_text (to_string !save))
	; hbox#add button
	; let button = new LTerm_widget.button
		~brackets:("[ ", " ]")
		"SAVE 3"
	in
	button#on_click (fun _ -> launchSave "save/save.itama3" save; savePrint#set_text (to_string !save))
	; hbox#add button
	; let button = new LTerm_widget.button
		~brackets:("[ ", " ]")
		"SAVE 4"
	in
		button#on_click (fun _ -> launchSave "save/save.itama4" save; savePrint#set_text (to_string !save))
	; hbox#add button
	; vbox#add hbox

	; let hbox = new LTerm_widget.hbox in
	 let button = new LTerm_widget.button
		~brackets:("[ ", " ]")
		"play"
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
	(fun () -> LTerm.disable_mouse term) >>= fun () -> main !save

let () = Lwt_main.run (save ())
