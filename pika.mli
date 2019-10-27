module Pika :
sig
	module Bar :
	sig
		type t

		val return : int -> t

	end

	type t

	val return : int * int * int * int -> t

	val raw : t -> int * int * int * int
	val eat : t -> t
	val thunder : t -> t
	val bath : t -> t
	val kill : t -> t
	val dance : t -> t

	val health : t -> int
	val energy : t -> int
	val hygiene : t -> int
	val happiness : t -> int

	val one_sec : t -> t

	class renderer : t ref -> LTerm_widget.spacing
	class bar_renderer : t ref -> LTerm_widget.label
end
