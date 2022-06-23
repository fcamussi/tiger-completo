structure tigertips =
struct

type unique = unit ref (* nos permite conocer si dos tipos son los mismos *)
datatype Tipo = TUnit (* no asignable *)
	| TNil
	| TInt of read (* read = RO para los indices de los for *)
	| TString
	| TArray of Tipo ref  * unique
	| TRecord of (string * Tipo ref * int) list * unique
	| TTipo of string (* sinonimos de tipo *)
and read = RO | RW

end
