structure tigerseman :> tigerseman =
struct

open tigerabs
open tigersres
open tigertemp
open tigertrans

type expty = {exp: unit, ty: Tipo}

type venv = (string, EnvEntry) tigertab.Tabla
type tenv = (string, Tipo) tigertab.Tabla

val levelPila: tigertrans.level tigerpila.Pila = tigerpila.nuevaPila1(tigertrans.outermost) 
fun pushLevel l = tigerpila.pushPila levelPila l
fun popLevel() = tigerpila.popPila levelPila 
fun topLevel() = tigerpila.topPila levelPila

(* tab_vars y tab_tipos contienen los valores iniciales para venv y tenv *)
val tab_tipos : (string, Tipo) Tabla = tabInserList(
	tabNueva(),
	[("int", TInt RW), ("string", TString)])
val tab_vars : (string, EnvEntry) Tabla = tabInserList(
	tabNueva(),
	[("print", Func{level=topLevel(), label="_print",
		formals=[TString], result=TUnit, extern=true}),
	("flush", Func{level=topLevel(), label="_flush",
		formals=[], result=TUnit, extern=true}),
	("getstr", Func{level=topLevel(), label="_getstr",
		formals=[], result=TString, extern=true}),
	("ord", Func{level=topLevel(), label="_ord",
		formals=[TString], result=TInt RW, extern=true}),
	("chr", Func{level=topLevel(), label="_chr",
		formals=[TInt RW], result=TString, extern=true}),
	("size", Func{level=topLevel(), label="_size",
		formals=[TString], result=TInt RW, extern=true}),
	("substring", Func{level=topLevel(), label="_substring",
		formals=[TString, TInt RW, TInt RW], result=TString, extern=true}),
	("concat", Func{level=topLevel(), label="_concat",
		formals=[TString, TString], result=TString, extern=true}),
	("not", Func{level=topLevel(), label="_not",
		formals=[TInt RW], result=TInt RW, extern=true}),
	("exit", Func{level=topLevel(), label="_exit",
		formals=[TInt RW], result=TUnit, extern=true})
	])

fun tipoReal (TTipo s, (env : tenv)) : Tipo = 
    (case tabBusca(s , env) of 
         NONE => raise Fail "tipoReal Ttipo"
       | SOME t => t)
  | tipoReal (t, _) = t

fun esEntero (TInt _) = true
  | esEntero _ = false;

fun tiposIguales (TRecord _) TNil = true
  | tiposIguales TNil (TRecord _) = true 
  | tiposIguales (TRecord (_, u1)) (TRecord (_, u2 )) = (u1=u2)
  | tiposIguales (TArray (_, u1)) (TArray (_, u2)) = (u1=u2)
  | tiposIguales (TTipo _) b =
		(* let *)
		(* 	val a = case !r of *)
		(* 		SOME t => t *)
		(* 		| NONE => raise Fail "No debería pasar! (1)" *)
		(* in *)
		(* 	tiposIguales a b *)
		(* end *)raise Fail "No debería pasar! (1)"
  | tiposIguales a (TTipo _) =
		(* let *)
		(* 	val b = case !r of *)
		(* 		SOME t => t *)
		(* 		| NONE => raise Fail "No debería pasar! (2)" *)
		(* in *)
		(* 	tiposIguales a b *)
		(* end *)raise Fail "No debería pasar! (2)"
  | tiposIguales (TInt _) (TInt _) = true
  | tiposIguales a b = (a=b)

fun sonNil (NilExp _) (NilExp _) = true
|   sonNil _ _ = false

fun transExp(venv, tenv) =
	let fun error(s, p) = raise Fail ("Error -- línea "^Int.toString(p)^": "^s^"\n")
		fun trexp(VarExp v) = trvar(v)
		| trexp(UnitExp _) = {exp=unitExp(), ty=TUnit}
		| trexp(NilExp _)= {exp=nilExp(), ty=TNil}
		| trexp(IntExp(i, _)) = {exp=intExp i, ty=TInt RW}
		| trexp(StringExp(s, _)) = {exp=stringExp(s), ty=TString}
		| trexp(CallExp({func, args}, nl)) =
            let val (level, label, formals, result, extern) = case tabBusca (func, venv) of
                    SOME (Func {level, label, formals, result, extern}) => (level, label, formals, result, extern)
                    | SOME _ => error (func^" no es una función", nl)
                    | NONE => error (func^" no está definida", nl)
                val letargs = List.map trexp args
                val ltargs = List.map (#ty) letargs
                val _ = if length ltargs > length formals then error ("Demasiados argumentos para "^func, nl) else ()
                val _ = if length ltargs < length formals then error ("Faltan argumentos para "^func, nl) else ()
                val iguales = List.map (fn(t1,t2) => tiposIguales t1 t2) (ListPair.zip (ltargs,formals))
                val _ = if not ((foldr (fn (x,y) => x andalso y) true) iguales)
                            then error ("Tipo incorrecto en algún argumento para "^func, nl)
                            else ()
            in
                {exp=callExp(label,extern,tiposIguales result TUnit,level,(List.map (#exp) letargs)), ty=result}
            end
		| trexp(OpExp({left, oper=EqOp, right}, nl)) =
			let
				val {exp=expl, ty=tyl} = trexp left
				val {exp=expr, ty=tyr} = trexp right
			in
				if tiposIguales tyl tyr andalso not (sonNil left right) andalso tyl<>TUnit then 
					{exp=if tiposIguales tyl TString then binOpStrExp {left=expl,oper=EqOp,right=expr} else binOpIntRelExp {left=expl,oper=EqOp,right=expr}, ty=TInt RW}
					else error("Tipos no comparables", nl)
			end
		| trexp(OpExp({left, oper=NeqOp, right}, nl)) = 
			let
				val {exp=expl, ty=tyl} = trexp left
				val {exp=expr, ty=tyr} = trexp right
			in
				if tiposIguales tyl tyr andalso not (sonNil left right) andalso tyl<>TUnit then 
					{exp=if tiposIguales tyl TString then binOpStrExp {left=expl,oper=NeqOp,right=expr} else binOpIntRelExp {left=expl,oper=NeqOp,right=expr}, ty=TInt RW}
					else error("Tipos no comparables", nl)
			end
		| trexp(OpExp({left, oper, right}, nl)) = 
			let
				val {exp=expl, ty=tyl} = trexp left
				val {exp=expr, ty=tyr} = trexp right
			in
				if tiposIguales tyl tyr then
					case oper of
						PlusOp => if esEntero(tipoReal(tyl, tenv)) then {exp=binOpIntExp {left=expl, oper=oper, right=expr},ty=TInt RW} else error("Error de tipos", nl)
						| MinusOp => if esEntero(tipoReal(tyl, tenv)) then {exp=binOpIntExp {left=expl, oper=oper, right=expr},ty=TInt RW} else error("Error de tipos", nl)
						| TimesOp => if esEntero(tipoReal(tyl, tenv)) then {exp=binOpIntExp {left=expl, oper=oper, right=expr},ty=TInt RW} else error("Error de tipos", nl)
						| DivideOp => if esEntero(tipoReal(tyl, tenv)) then {exp=binOpIntExp {left=expl, oper=oper, right=expr},ty=TInt RW} else error("Error de tipos", nl)
						| LtOp => if esEntero(tipoReal(tyl, tenv)) orelse tipoReal(tyl,tenv)=TString then
							{exp=if esEntero(tipoReal(tyl, tenv)) then binOpIntRelExp {left=expl,oper=oper,right=expr} else binOpStrExp {left=expl,oper=oper,right=expr},ty=TInt RW} 
							else error("Error de tipos", nl)
						| LeOp => if esEntero(tipoReal(tyl, tenv)) orelse tipoReal(tyl,tenv)=TString then 
							{exp=if esEntero(tipoReal(tyl, tenv)) then binOpIntRelExp {left=expl,oper=oper,right=expr} else binOpStrExp {left=expl,oper=oper,right=expr},ty=TInt RW} 
							else error("Error de tipos", nl)
						| GtOp => if esEntero(tipoReal(tyl, tenv)) orelse tipoReal(tyl,tenv)=TString then
							{exp=if esEntero(tipoReal(tyl, tenv)) then binOpIntRelExp {left=expl,oper=oper,right=expr} else binOpStrExp {left=expl,oper=oper,right=expr},ty=TInt RW} 
							else error("Error de tipos", nl)
						| GeOp => if esEntero(tipoReal(tyl, tenv)) orelse tipoReal(tyl,tenv)=TString then
							{exp=if esEntero(tipoReal(tyl, tenv)) then binOpIntRelExp {left=expl,oper=oper,right=expr} else binOpStrExp {left=expl,oper=oper,right=expr},ty=TInt RW} 
							else error("Error de tipos", nl)
						| _ => raise Fail "No debería pasar! (3)"
				else error("Error de tipos", nl)
			end
		| trexp(RecordExp({fields, typ}, nl)) =
			let
				(* Traducir cada expresión de fields *)
				val tfields = map (fn (sy,ex) => (sy, trexp ex)) fields

				(* Buscar el tipo *)
				val (tyr, cs) = case tabBusca(typ, tenv) of
					SOME t => (case tipoReal(t,tenv) of
						TRecord (cs, u) => (TRecord (cs, u), cs)
						| _ => error(typ^" no es de tipo record", nl))
					| NONE => error("Tipo inexistente ("^typ^")", nl)
				
                (* Orden según la posición en cs para Listsort.sort *)
                fun orden ((sy,_),(sy',_)) = let
                    val aux = fn sy => #3 (hd (List.filter (fn (s,_,_) => s = sy) cs))
                        handle Empty => error ("No existe el campo " ^ sy, nl)
                in
                    Int.compare(aux sy, aux sy')
                end

				(* Verificar que cada campo esté en orden y tenga una expresión del tipo que corresponde *)
				fun verificar [] [] = []
				  | verificar (c::cs) [] = error("Faltan campos", nl)
				  | verificar [] (c::cs) = error("Sobran campos", nl)
				  | verificar ((s,t,_)::cs) ((sy,{exp,ty})::ds) =
						if s<>sy then error("Error de campo", nl)
						else if tiposIguales ty (!t) then exp::(verificar cs ds)
							 else error("Error de tipo del campo "^s, nl)
				val lf = verificar cs (Listsort.sort orden tfields)
			in
				{exp=recordExp lf, ty=tyr}
			end
		| trexp(SeqExp(s, nl)) =
			let	val lexti = map trexp s
				val exprs = map (fn{exp, ty} => exp) lexti
				val {exp, ty=tipo} = hd(rev lexti)
			in	{ exp=seqExp (exprs), ty=tipo } end
		| trexp(AssignExp({var=SimpleVar s, exp}, nl)) = 
            let
                val _ = case tabBusca (s, venv) of
                            SOME (Var {ty=TInt RO, access, level}) => error ("La variable "^s^" es de solo lectura", nl)
                            | _ => ()
                val {ty=tyder,exp=expder} = trexp exp
                val {ty=tyvar,exp=expvar} = trvar ((SimpleVar s), nl)
            in
                if tiposIguales tyder tyvar
                    then {exp=assignExp{var=expvar, exp=expder}, ty=TUnit}
                    else error ("Tipos distintos en la asignación", nl)
            end
		| trexp(AssignExp({var, exp}, nl)) =
            let
                val {ty=tyder,exp=expder} = trexp exp
                val {ty=tyvar,exp=expvar} = trvar (var, nl)
            in
                if tiposIguales tyder tyvar
                    then {exp=assignExp{var=expvar, exp=expder}, ty=TUnit}
                    else error ("Tipos distintos en la asignación", nl)
            end
		| trexp(IfExp({test, then', else'=SOME else'}, nl)) =
			let val {exp=testexp, ty=tytest} = trexp test
			    val {exp=thenexp, ty=tythen} = trexp then'
			    val {exp=elseexp, ty=tyelse} = trexp else'
			in
				if esEntero(tipoReal(tytest,tenv)) andalso tiposIguales tythen tyelse then
				{exp=if tipoReal(tythen,tenv)=TUnit then ifThenElseExpUnit {test=testexp,then'=thenexp,else'=elseexp} else ifThenElseExp {test=testexp,then'=thenexp,else'=elseexp}, ty=tythen}
				else error("Error de tipos en if" ,nl)
			end
		| trexp(IfExp({test, then', else'=NONE}, nl)) =
			let val {exp=exptest,ty=tytest} = trexp test
			    val {exp=expthen,ty=tythen} = trexp then'
			in
				if esEntero(tipoReal(tytest,tenv)) andalso tythen=TUnit then
				{exp=ifThenExp{test=exptest, then'=expthen}, ty=TUnit}
				else error("Error de tipos en if", nl)
			end
		| trexp(WhileExp({test, body}, nl)) =
			let
				val ttest = trexp test
                val _ = preWhileForExp ()
				val tbody = trexp body
				val {exp=exp', ty=ty'} = if esEntero(tipoReal(#ty ttest, tenv)) andalso #ty tbody = TUnit then {exp=whileExp {test=(#exp ttest), body=(#exp tbody), lev=topLevel()}, ty=TUnit}
				    else if not (esEntero(tipoReal(#ty ttest, tenv))) then error("Error de tipo en la condición", nl)
				    else error("El cuerpo de un while no puede devolver un valor", nl)
                val _ = postWhileForExp()
			in
                {exp=exp',ty=ty'}
			end
		| trexp(ForExp({var, escape, lo, hi, body}, nl)) =
            let
                val {exp=loexp,ty=tylo} = trexp lo
                val {exp=hiexp,ty=tyhi} = trexp hi
                val _ = if not (esEntero(tipoReal(tylo,tenv))) then error("Cota inferior no es de tipo entero", nl) else ()
                val _ = if not (esEntero(tipoReal(tyhi,tenv))) then error("Cota superior no es de tipo entero", nl) else ()
            	val access = allocLocal (topLevel()) (!escape)
				val level = getActualLev()
                (* tabRInserta <- sobreescribe si existe *)
                val venv' = tabRInserta (var, (Var {ty=TInt RO, access=access, level=level}), fromTab venv)
                val _ = preWhileForExp()
                val {exp=bodyexp, ty=tybody} = transExp (venv',tenv) body
                val {exp=exp', ty=ty'} = if not (tybody = TUnit) then error("El cuerpo del for no es de tipo unit",nl) else {exp=forExp {lo=loexp, hi=hiexp, var=simpleVar(access,level), body=bodyexp}, ty=TUnit}
                val _ = postWhileForExp()
            in
                {exp=exp',ty=ty'}                
            end
		| trexp(LetExp({decs, body}, _)) =
			let
				fun aux (d, (v, t, exps1)) =
				let
					val (v', t', exps2) = trdec (v, t) d
				in
					(v', t', exps1@exps2)
				end
				val (venv', tenv', expdecs) = List.foldl aux (venv, tenv, []) decs
				val {exp=expbody,ty=tybody}=transExp (venv', tenv') body
			in 
				{exp=seqExp(expdecs@[expbody]), ty=tybody}
			end
		| trexp(BreakExp nl) = ({exp = breakExp(), ty=TUnit} handle breakexc => error ("break fuera del ciclo", nl))
		| trexp(ArrayExp({typ, size, init}, nl)) =
            let
                val (tya, tyelem) = case tabBusca (typ, tenv) of
                    SOME t => (case tipoReal(t,tenv) of
                        TArray (t,u) => (TArray (t,u), t)
                        | _ => error(typ^" no es de tipo array", nl))
                    | NONE => error("Tipo inexistente "^typ, nl)
                val {exp=expsize,ty=tysize} = trexp size
                val _ = if not (esEntero(tipoReal(tysize, tenv)))
                    then error("El tipo de la expresion para el tamaño del array no es entero", nl)
                    else ()
                val {exp=expinit,ty=tyinit} = trexp init
                val _ = if not (tiposIguales (!tyelem) tyinit)
                    then error("El tipo del array no coincide con el de inicialización",nl)
                    else ()
            in
                {exp=arrayExp{size=expsize, init=expinit}, ty=tya}
            end
		and trvar(SimpleVar s, nl) = (case tabBusca (s, venv) of
                                          SOME (Var {ty=ty, access=access, level=level}) => {exp=simpleVar(access, level), ty=ty}
                                          | SOME _ => error (s^" no es una variable", nl)
                                          | NONE => error (s^" no está definida", nl))
        | trvar(FieldVar(v, s), nl) = let
                val {exp=expvar,ty=ty} = trvar(v, nl)
                val ltr = case ty of
                    TRecord(l,u) => l
                    | _ => error ("No es un record", nl)
                val (tym,pos) = case List.find (fn x => (#1) x = s) ltr of
                    SOME x => ((#2) x, (#3) x)
                    | NONE => error (s^" no es un miembro", nl)
            in {exp=fieldVar(expvar,pos), ty=(!tym)}
        end
		| trvar(SubscriptVar(v, e), nl) = let
                val {exp=expvar,ty=ty} = trvar(v, nl)
                val ta = case ty of
                    TArray (ta,u) => ta
                    | _ => error ("No es un array", nl)
                    val {exp=expind,ty=te} = trexp e
                    val _ = if not (esEntero(tipoReal(te,tenv))) then
                               error ("El indice tiene que ser de tipo entero",nl)
                            else ()
            in
                {exp=subscriptVar(expvar,expind), ty=(!ta)}
            end
		and trdec (venv, tenv) (VarDec ({name,escape,typ=NONE,init},pos)) = 
            let
                val {exp=expinit,ty=tyinit} = transExp (venv,tenv) init
                (* TUnit no es asignable y TNil solo puede asignarse a records *)
                val _ = if (tiposIguales tyinit TUnit orelse tyinit = TNil)
                            then error ("El tipo de la expresión de inicialización es erroneo", pos)
                            else ()
                val acc = allocLocal (topLevel()) (!escape)
                val el = assignExp{var=varDec(acc), exp=expinit} (* efecto lateral *)
                val venv' = tabRInserta (name, Var {ty=tyinit, access=acc, level=(getActualLev())}, venv)
            in
                (venv', tenv, [el])
            end
		| trdec (venv,tenv) (VarDec ({name,escape,typ=SOME s,init},pos)) =
            let
                val {exp=expinit,ty=tyinit} = transExp (venv,tenv) init
                val ty = case tabBusca(s, tenv) of
                             SOME ty => ty
                             | NONE => error ("No existe el tipo "^s, pos)
                val _ = if not (tiposIguales ty tyinit) then
                            error ("El tipo de la expresión de inicialización no es de tipo "^s, pos)
                        else ()
                val acc = allocLocal (topLevel()) (!escape)
                val el = assignExp{var=varDec(acc), exp=expinit} (* efecto lateral *)
                val venv' = tabRInserta (name, Var {ty=ty, access=acc, level=(getActualLev())}, venv)
            in
                (venv', tenv, [el])
            end
		| trdec (venv,tenv) (FunctionDec fs) =
            let
                fun trty (NameTy s,pos) = (case tabBusca(s,tenv) of
                        SOME ty => ty
                        | NONE => error ("No existe el tipo "^s, pos))
                | trty _ = raise Fail "error interno en tigerseman.trdec (1)"

                fun reps [] = ()
                |   reps (({name, params, result, body}, pos)::fs) =
                        if List.exists (fn ({name=name', params, result, body},pos) => name = name') fs
                            then error("Nombre de función repetido " ^ name, pos)
                            else reps fs

                (* checkty: checkea tipos y carga frame *)
                fun checkty venv [] = ()
                |   checkty venv (({name, params, result, body}, pos)::fs) = let
                        val (label, formals, result) = case tabBusca(name,venv) of
                                     SOME (Func x) => (#label x, #formals x, #result x)
                                     | _ => raise Fail "error interno en tigerseman.trdec (2)"
                        val _ = preFunctionDec()
                        val level = newLevel {parent=topLevel(), name=label, formals = map (! o #escape) params}
                        val _ = pushLevel level
                        val access = tigertrans.formals level
                        val venv' = tabInserList (venv, map (fn (par,acc) => (#name par, Var {ty=trty (#typ par,pos),
 access=acc, level=(getActualLev())})) (ListPair.zip (params,access)))
                        val {exp=funexp,ty=tybody} = transExp (venv',tenv) body
                        val _ = if not (tiposIguales result tybody)
                                    then error ("Tipo de la función "^name^" es incorrecta", pos)
                                    else ()
                        val _ = functionDec(funexp, topLevel(), tiposIguales result TUnit)
                        val _ = popLevel()
                        val _ = postFunctionDec()
                    in
                        checkty venv fs
                    end

                val _ = reps fs

                (* cargo batch de funciones *)
                val lpr = map (fn ({name, params, result, body}, pos) => (
                                  name,
                                  Func {level = topLevel(),
                                        label = ("_" ^ newlabel()) ^ "_" ^ name,
                                        formals = map (fn {name, escape, typ} => trty (typ,pos)) params,
                                        (* si no se especificó result es unit *)
                                        result = (fn SOME s => trty (NameTy s, pos) | NONE => TUnit) result,
                                        extern = false})) fs
                val venv' = tabInserList (venv, lpr)

                val _ = checkty venv' fs
            in
			    (venv', tenv, [])
            end
		| trdec (venv,tenv) (TypeDec ts) = let
                fun reps [] = ()
                |   reps (({name,ty},pos)::ts) = if List.exists (fn ({name=name',ty},pos) => name = name') ts
                                      then error("Nombre de tipo repetido " ^ name, pos)
                                      else reps ts
                val _ = reps ts
                val ts' = map #1 ts
                val tenv' = (tigertopsort.fijaTipos ts' tenv)
                                handle tigertopsort.Ciclo => error ("Hay un ciclo en la declaración de tipos", #2 (hd ts))
                                | tigertopsort.NoExiste s => error ("No existe el tipo "^s, #2 (hd ts))
            in
			    (venv, tenv', [])
            end
	in trexp end

fun transProg ex =
	let	val main =
				LetExp({decs=[FunctionDec[({name="tigermain", params=[],
								result=SOME "int", body=ex}, 0)]],
						body=UnitExp 0}, 0)
		val _ = transExp(tab_vars, tab_tipos) main
	in	print "bien!\n" end
end
