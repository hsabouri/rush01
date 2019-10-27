type t = (int * int * int * int)

val openSave : string -> t option
val writeSave : t -> string -> unit
