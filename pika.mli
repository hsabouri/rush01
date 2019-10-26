module Pika :
sig
	module Bar :
	sig
		type t
	end

	type t

	val return : int * int * int * int -> t

	val eat : t -> t
	val thunder : t -> t
	val bath : t -> t
	val kill : t -> t

	val health : t -> int
	val energy : t -> int
	val hygiene : t -> int
	val happiness : t -> int

	val one_sec : t -> t
end