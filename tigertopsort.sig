signature tigertopsort =
sig
	exception Ciclo
    exception NoExiste of string
	val fijaTipos : {name : string, ty : tigerabs.ty} list ->
		(string, tigertips.Tipo) tigertab.Tabla ->
		(string, tigertips.Tipo) tigertab.Tabla
end
