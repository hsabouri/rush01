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
	end

	let (|+|) = Bar.(|+|)
	let (|-|) = Bar.(|-|)

	type t = Bar.t * Bar.t * Bar.t * Bar.t

	let return (he, en, hy, ha) = (
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

end
