signature tigertemp = sig
	type label = string
	type temp = string
	val makeString: string -> string
	val newtemp: unit -> temp
	val newlabel: unit -> label
    val compare: string * string -> order
end
