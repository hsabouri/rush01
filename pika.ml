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

	let eat (he, en, hy, ha) = (
		he |+| 25,
		en |-| 10,
		hy |-| 20,
		ha |+| 5
	)

	let thunder (he, en, hy, ha) = (
		he |-| 20,
		en |+| 25,
		hy,
		ha |-| 20
	)

	let bath (he, en, hy, ha) = (
		he |-| 20,
		en |-| 10,
		hy |+| 25,
		ha |+| 5
	)

	let kill (he, en, hy, ha) = (
		he |-| 20,
		en |-| 10,
		hy,
		ha |+| 20
	)

	let health (he, _, _, _) = he
	let energy (_, en, _, _) = en
	let hygiene (_, _, hy, _) = hy
	let happiness (_, _, _, ha) = ha

	let one_sec (he, en, hy, ha) = (he |-| 1, en, hy, ha)

	let get_state (he, en, hy, ha) =
		(he < 20, en < 20, hy < 20, ha < 20)

	let render ctx size t = match get_state t with
		| (sick, true, dirty, sad) -> Render.draw_image ctx size (Render.filter Render.pika_tired (sick, dirty, sad))
		| (sick, tired, dirty, sad) -> Render.draw_image ctx size (Render.filter Render.pika (sick, dirty, sad))

	class renderer (t: t ref) = object ( self )
		inherit LTerm_widget.spacing ~rows:20 () as super

		val mutable style = Render.background

		val t = t

		method! draw ctx _focused =
			let size: LTerm_geom.size = LTerm_draw.size ctx in
			LTerm_draw.fill_style ctx style
			; render ctx size !t (* personnal function that draw pixel per pixel a string with colors *)
	end

	let list_of_t (he, en, hy, ha) = [
		(he, Zed_string.of_utf8 "HEALTH")
		; (en, Zed_string.of_utf8 "ENERGY")
		; (hy, Zed_string.of_utf8 "HYGIENE")
		; (ha, Zed_string.of_utf8 "HAPPINESS")
	]

	class bar_renderer (t: t ref) = object ( self )
		inherit LTerm_widget.spacing ~rows:10 () as super

		val style = LTerm_style.none
		val mutable connection = LTerm_draw.Light

		val t = t

		method! draw ctx _focused =
			let l = list_of_t !t in
			let size = LTerm_draw.size ctx in
			LTerm_draw.fill_style ctx style
			; LTerm_draw.draw_hline ctx (size.rows / 2) 0 (LTerm_draw.size ctx).cols connection
			; let rec loop l i = match l with
				| elem :: tail -> Bar.render ctx size elem i ; loop tail (i + 1)
				| [] -> ()
			in loop l 0
	end
end
