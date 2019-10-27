module Pika =
struct
	module Bar =
	struct
		type t = int

		let zero = 0

		let return i = zero + match i with
			| i when i <= 100 && i >= 0 -> i
			| i when i > 100 -> 100
			| _ -> 0

		let (|+|) t i = return (t + i)
		let (|-|) t i = return (t - i)

		let translate_to t width =
			let t = float_of_int t in
			let width = float_of_int width in
			int_of_float ((t /. 100.0) *. width)

		let render ctx (size: LTerm_geom.size) (t, name) n =
			let t_sized = translate_to t (size.cols) in
			match t with
				| t when t > 20 -> Render.draw_bar ctx size Render.green (t_sized, name) n
				| t -> Render.draw_bar ctx size Render.red (t_sized, name) n

	end

	let (|+|) = Bar.(|+|)
	let (|-|) = Bar.(|-|)

	type t = Bar.t * Bar.t * Bar.t * Bar.t
	type state = bool * bool * bool * bool

	let return (he, en, hy, ha) : t = (
		Bar.return he,
		Bar.return en,
		Bar.return hy,
		Bar.return ha
	)

	let raw (he, en, hy, ha) : (int * int * int * int) = (
		he,
		en,
		hy,
		ha
	)

	let is_dead (he, en, hy, ha) = ha = 0 || en = 0 || hy = 0 || he = 0

	let eat (he, en, hy, ha) = match is_dead (he, en, hy, ha) with
		| false -> (
				he |+| 25,
				en |-| 10,
				hy |-| 20,
				ha |+| 5
			)
		| true -> (he, en, hy, ha)

	let thunder (he, en, hy, ha) = match is_dead (he, en, hy, ha) with
		| false -> (
			he |-| 20,
			en |+| 25,
			hy,
			ha |-| 20
		)
		| true -> (he, en, hy, ha)

	let bath (he, en, hy, ha) = match is_dead (he, en, hy, ha) with
		| false -> (
			he |-| 20,
			en |-| 10,
			hy |+| 25,
			ha |+| 5
		)
		| true -> (he, en, hy, ha)

	let kill (he, en, hy, ha) = match is_dead (he, en, hy, ha) with
		| false -> (
			he |-| 20,
			en |-| 10,
			hy,
			ha |+| 20
		)
		| true -> (he, en, hy, ha)

	let dance (he, en, hy, ha) = match is_dead (he, en, hy, ha) with
		| false -> (
			he |+| 10,
			en |-| 10,
			hy |-| 10,
			ha |+| 30
		)
		| true -> (he, en, hy, ha)

	let health (he, _, _, _) = he
	let energy (_, en, _, _) = en
	let hygiene (_, _, hy, _) = hy
	let happiness (_, _, _, ha) = ha

	let one_sec (he, en, hy, ha) = (he |-| 1, en, hy, ha)

	let get_state (he, en, hy, ha) =
		(he <= 20, en <= 20, hy <= 20, ha <= 20, is_dead (he, en, hy, ha))

	let choose_image animation tired sad dirty sick = match (animation, tired, sad, sick, dirty) with
		| (1, false, true, _, _) -> Render.pika_sad
		| (0, false, true, _, _) -> Render.pika_sad2
		| (1, false, _, _, true) -> Render.pika_dirty
		| (0, false, _, _, true) -> Render.pika_dirty2
		| (1, false, _, true, _) -> Render.pika_sick
		| (0, false, _, true, _) -> Render.pika_sick2
		| (1, false, false, _, _) -> Render.pika2
		| (0, true, _, _, _) -> Render.pika_tired
		| (1, true, _, _, _) -> Render.pika_tired2
		| (_, _, _, _, _) -> Render.pika2

	let render ctx size t animation = match get_state t with
		| (_, _, _, true, false) -> Render.draw_image ctx size (choose_image animation false true false false)
		| (true, _, _, _, false) -> Render.draw_image ctx size (choose_image animation false false false true)
		| (_, true, _, _, false) -> Render.draw_image ctx size (choose_image animation true false false false)
		| (_, _, true, _, false) -> Render.draw_image ctx size (choose_image animation false false true false)
		| (_, _, _, _, false) -> Render.draw_image ctx size (choose_image animation false false false false)
		| (_, _, _, _, true) -> Render.draw_image ctx size Render.pika_dead

	class renderer (t: t ref) = object ( self )
		inherit LTerm_widget.spacing ~rows:15 () as super

		val mutable style = Render.background

		val t = t
		val mutable animation = 0

		method! draw ctx _focused =
			let size: LTerm_geom.size = LTerm_draw.size ctx in
			LTerm_draw.fill_style ctx style
			; render ctx size !t animation (* personnal function that draw pixel per pixel a string with colors *)
			; animation <- if animation = 0 then 1 else 0
	end

	let list_of_t (he, en, hy, ha) = [
		(he, Zed_string.of_utf8 "HEALTH")
		; (en, Zed_string.of_utf8 "ENERGY")
		; (hy, Zed_string.of_utf8 "HYGIENE")
		; (ha, Zed_string.of_utf8 "HAPPINESS")
	]

	class bar_renderer (t: t ref) = object ( self )
		inherit LTerm_widget.label "" as super

		val style = LTerm_style.none
		val mutable connection = LTerm_draw.Light

		val t = t

		method! draw ctx _focused =
			let l = list_of_t !t in
			let size = LTerm_draw.size ctx in
			LTerm_draw.fill_style ctx style
			; let rec loop l i = match l with
				| elem :: tail -> Bar.render ctx size elem i ; loop tail (i + 1)
				| [] -> ()
			in loop l 0
	end
end
