structure tigertrans :> tigertrans = struct

open tigerframe
open tigertree
open tigertemp
open tigerabs

exception breakexc
exception divCero
	
type level = {parent:frame option , frame: frame, level: int}
type access = tigerframe.access

type frag = tigerframe.frag
(*val fraglist = ref ([]: frag list)*)

val actualLevel = ref ~1 (* L0_tigermain debe tener level = 0. *)
fun getActualLev() = !actualLevel

val outermost: level = {parent=NONE,
	frame=newFrame{name="L0_tigermain", formals=[]}, level=getActualLev()}
fun newLevel{parent={parent, frame, level}, name, formals} =
	{
	parent=SOME frame,
	frame=newFrame{name=name, formals=formals},
	level=level+1}
fun allocArg{parent, frame, level} b = tigerframe.allocArg frame b
fun allocLocal{parent, frame, level} b = tigerframe.allocLocal frame b
fun formals{parent, frame, level} = tl(tigerframe.formals frame) (* quito el static link de los argumentos *)

datatype exp =
	Ex of tigertree.exp (* Ex -> devuelve un valor *)
	| Nx of tigertree.stm (* Nx -> no devuelve ningun valor *)
	| Cx of label * label -> tigertree.stm (* Cx -> devuelve una condición *)

fun seq [] = EXP (CONST 0)
	| seq [s] = s
	| seq (x::xs) = SEQ (x, seq xs)

fun unEx (Ex e) = e
	| unEx (Nx s) = ESEQ(s, CONST 0)
	| unEx (Cx cf) =
	let
		val r = newtemp()
		val t = newlabel()
		val f = newlabel()
	in
		ESEQ(seq [MOVE(TEMP r, CONST 1),
			cf (t, f),
			LABEL f,
			MOVE(TEMP r, CONST 0),
			LABEL t],
			TEMP r)
	end

fun unNx (Ex e) = EXP e
	| unNx (Nx s) = s
	| unNx (Cx cf) =
	let
		val t = newlabel()
		val f = newlabel()
	in
		seq [cf(t,f),
			LABEL t,
			LABEL f]
	end

fun unCx (Nx s) = raise Fail ("Error (UnCx(Nx..))")
	| unCx (Cx cf) = cf
	| unCx (Ex (CONST 0)) =
	(fn (t,f) => JUMP(NAME f, [f]))
	| unCx (Ex (CONST _)) =
	(fn (t,f) => JUMP(NAME t, [t]))
	| unCx (Ex e) =
	(fn (t,f) => CJUMP(NE, e, CONST 0, t, f))

fun Ir(e) =
	let	fun aux(Ex e) = tigerit.tree(EXP e)
		| aux(Nx s) = tigerit.tree(s)
		| aux _ = raise Fail "bueno, a completar!"
		fun aux2(PROC{body, frame}) = (tigerframe.name frame)^":\n"^(aux(Nx body))
                                      ^"-----------------------------\n"
		| aux2(STRING(l, "")) = l^":\n"
		| aux2(STRING("", s)) = "\t"^s^"\n"
		| aux2(STRING(l, s)) = l^":\t"^s^"\n"
		fun aux3 [] = ""
		| aux3(h::t) = (aux2 h)^(aux3 t)
	in	aux3 e end
(*fun nombreFrame frame = print(".globl " ^ tigerframe.name frame ^ "\n")*)

(* While y for necesitan la u'ltima etiqueta para un break *)
local
	val salidas: label option tigerpila.Pila = tigerpila.nuevaPila1 NONE
in
	val pushSalida = tigerpila.pushPila salidas
	fun popSalida() = tigerpila.popPila salidas
	fun topSalida() =
		case tigerpila.topPila salidas of
		SOME l => l
		| NONE => raise breakexc
end

val datosGlobs = ref ([]: frag list)

fun procEntryExit{level: level, body} =
	let	
		val body' = PROC{frame= #frame level, body=unNx body}
	in	datosGlobs:=(!datosGlobs@[body']) end
fun getResult() = !datosGlobs

fun stringExp(s: string) =
	let	val l = newlabel()
		val _ = datosGlobs:=(!datosGlobs @ [STRING(l, s)])
	in	Ex(NAME l) end
fun preFunctionDec() =
	(pushSalida(NONE);
	actualLevel := !actualLevel+1)
fun functionDec(e, l, proc) =
	let	val body =
				if proc then unNx e
				else MOVE(TEMP rv, unEx e)
		val body' = procEntryExit1(body, #frame l)
		val () = procEntryExit{body=Nx body', level=l}
	in	Ex(CONST 0) end
fun postFunctionDec() =
	(popSalida(); actualLevel := !actualLevel-1)

fun unitExp() = Ex (CONST 0)

fun nilExp() = Ex (CONST 0)

fun intExp i = Ex (CONST i)

(* InReg -> temporal
   InFrame -> memoria: si nivel = getActualLev() está en el mismo frame, sino está en otro *)
fun simpleVar(acc, nivel) = case acc of
                    InReg reg => Ex(TEMP reg)
                    | InFrame offset => let
                            fun aux 0 = TEMP tigerframe.fp
                            | aux n = MEM(BINOP(PLUS, CONST tigerframe.fpPrevLev, aux(n-1)))
                        in Ex(MEM(BINOP(PLUS, CONST offset, aux(getActualLev() - nivel))))
                    end

fun varDec(acc) = simpleVar(acc, getActualLev())

fun fieldVar(var, field) = let
        val t = newtemp()
    in
        Ex(ESEQ(seq[MOVE(TEMP t, unEx var),
                    EXP(externalCall("_checkNil", [TEMP t]))],
                    MEM(BINOP(PLUS, TEMP t, CONST(tigerframe.wSz * field)))))
    end

fun subscriptVar(arr, ind) =
let
	val a = unEx arr
	val i = unEx ind
	val ra = newtemp()
	val ri = newtemp()
in
	Ex( ESEQ(seq[MOVE(TEMP ra, a),
		MOVE(TEMP ri, i),
		EXP(externalCall("_checkindex", [TEMP ra, TEMP ri]))],
		MEM(BINOP(PLUS, TEMP ra,
			BINOP(MUL, TEMP ri, CONST tigerframe.wSz)))))
end

fun recordExp lf = let (* lf ya viene ordenado por posición *)
        val t = newtemp()
        val n = CONST (length lf)
        val lfe = List.map unEx lf
    in
        Ex(ESEQ(SEQ(EXP(externalCall("_createRecord", n::lfe)),
                        MOVE(TEMP t, TEMP tigerframe.rv)), TEMP t))
    end

fun arrayExp{size, init} = let
        val t = newtemp()
    in
        Ex(ESEQ(SEQ(EXP(externalCall("_createArray", [unEx size, unEx init])),
                        MOVE(TEMP t, TEMP tigerframe.rv)), TEMP t))
    end

fun callExp (name,external,isproc,lev:level,ls) = let
        val params = List.map unEx ls

        (* static link *)
        val sl = let fun aux 0 = TEMP tigerframe.fp 
		       	     | aux n = MEM(BINOP(PLUS, CONST tigerframe.fpPrevLev, aux(n-1)))
                 in aux(getActualLev() - (#level lev)) end

        val call = if external then EXP(externalCall(name, params))
                   else EXP(CALL(NAME name, [sl]@params))

        val t = newtemp()
    in
        (* si es un proceso no devuelve nada pero si es una funcion devuelve el registro rv *)
    	if isproc then Nx(call)
                  else Ex(ESEQ(SEQ(call, MOVE(TEMP t, TEMP tigerframe.rv)), TEMP t))
    end

(*fun letExp ([], body) = Ex (unEx body)
 |  letExp (inits, body) = Ex (ESEQ(seq inits, unEx body))*)

fun breakExp() = let
        val l = topSalida()
    in
        Nx(JUMP(NAME l, [l]))
    end

fun seqExp([]: exp list) = Nx (EXP(CONST 0))
	| seqExp (exps: exp list) =
		let
			fun unx [e] = []
				| unx (s::ss) = (unNx s)::(unx ss)
				| unx[] = []
		in
			case List.last exps of
				Nx s =>
					let val unexps = map unNx exps
					in Nx (seq unexps) end
				| Ex e => Ex (ESEQ(seq(unx exps), e))
				| cond => Ex (ESEQ(seq(unx exps), unEx cond))
		end

fun preWhileForExp() = pushSalida(SOME(newlabel()))

fun postWhileForExp() = (popSalida(); ())

fun whileExp {test: exp, body: exp, lev:level} =
let
	val cond = unCx test
	val (l1, l2, l3) = (newlabel(), newlabel(), topSalida())
in
	Nx (seq[LABEL l1,
		cond(l2,l3),
		LABEL l2,
		unNx body,
		JUMP(NAME l1, [l1]),
		LABEL l3])
end

fun forExp {lo, hi, var, body} = let
        val var' = unEx var
        val (l1, l2, sal) = (newlabel(), newlabel(), topSalida())
    in
        Nx(seq(case hi of
            Ex(CONST n) => if n < maxInt
                (* separamos el caso n < maxInt y n = maxInt para que sea más óptimo *)
                then [MOVE(var', unEx lo),
                      JUMP(NAME l2, [l2]),
                      LABEL l1,
                      unNx body,
                      MOVE(var', BINOP(PLUS, var', CONST 1)),
                      LABEL l2,
                      CJUMP(GT, var', CONST n, sal, l1),
                      LABEL sal]
                else [MOVE(var', unEx lo),
                      LABEL l2,
                      CJUMP(GT, var', CONST n, sal, l1),
                      LABEL l1,
                      unNx body,
                      MOVE(var', BINOP(PLUS, var', CONST 1)),
                      JUMP(NAME l2, [l2]),
                      LABEL sal]
            | hi => let val t = newtemp () in (* éste temp es por si hi no es una constante y su valor se modifica en el body del for *)
                (* acá los casos n < maxInt y n = maxInt no los separamos *)
                 [MOVE(var', unEx lo),
                  MOVE(TEMP t, unEx hi),
                  CJUMP(LE, var', TEMP t, l2, sal),
                  LABEL l2,
                  unNx body,
                  CJUMP(EQ, TEMP t, var', sal, l1),
                  LABEL l1,
                  MOVE(var', BINOP(PLUS, var', CONST 1)),
                  JUMP(NAME l2, [l2]),
                  LABEL sal]
            end))
    end

fun ifThenExp{test, then'} = let
        val cond = unCx test
        val (l1, l2) = (newlabel(), newlabel())
    in
        Nx(seq[cond(l1,l2),
               LABEL l1,
               unNx then',
               LABEL l2])
    end

fun ifThenElseExp {test, then',else'} = let
        val cond = unCx test
        val (l1 ,l2, l3) = (newlabel(), newlabel(), newlabel())
        val t = newtemp()
    in
        Ex(ESEQ(seq[cond(l1,l2),
                    LABEL l1,
                    SEQ(MOVE(TEMP t, unEx then'), JUMP(NAME l3, [l3])),
                    LABEL l2,
                    MOVE(TEMP t, unEx else'),
                    LABEL l3],
                    TEMP t))
    end

fun ifThenElseExpUnit {test, then', else'} = let
        val cond = unCx test
        val (l1 ,l2, l3) = (newlabel(), newlabel(), newlabel())
    in
        Nx(seq[cond(l1,l2),
           LABEL l1,
           SEQ(unNx then', JUMP(NAME l3, [l3])),
           LABEL l2,
           unNx else',
           LABEL l3])
    end

fun assignExp{var, exp} = Nx(MOVE(unEx var, unEx exp))

fun binOpIntExp {left, oper, right} = let
        val left' = unEx left
        val right' = unEx right
        val t = newtemp()
    in
        case oper of
            tigerabs.PlusOp => Ex(ESEQ(MOVE(TEMP t, BINOP(PLUS, left', right')), TEMP t))
            | tigerabs.MinusOp => Ex(ESEQ(MOVE(TEMP t, BINOP(MINUS, left', right')), TEMP t))
            | tigerabs.TimesOp => Ex(ESEQ(SEQ(EXP(externalCall("_mul", [left', right'])), MOVE(TEMP t, TEMP tigerframe.rv)), TEMP t))
            | tigerabs.DivideOp => Ex(ESEQ(SEQ(EXP(externalCall("_div", [left', right'])), MOVE(TEMP t, TEMP tigerframe.rv)), TEMP t))
            | _ => raise Fail "error interno en tigertrans.binOpIntExp"        
    end

fun binOpIntRelExp {left,oper,right} = let
        val (l1 ,l2) = (newlabel(), newlabel())
        val oper' = case oper of
                        tigerabs.EqOp => EQ
                        | tigerabs.NeqOp => NE
                        | tigerabs.LtOp => LT
                        | tigerabs.LeOp => LE
                        | tigerabs.GtOp => GT
                        | tigerabs.GeOp => GE
                        | _ => raise Fail "error interno en tigertrans.binOpIntRelExp"
    in
        Cx (fn (l1,l2) => CJUMP(oper', unEx left, unEx right, l1, l2))
    end

fun binOpStrExp {left,oper,right} = let
        val t = newtemp()
        val (l1 ,l2) = (newlabel(), newlabel())
        val oper' = case oper of
                        tigerabs.EqOp => EQ
                        | tigerabs.NeqOp => NE
                        | tigerabs.LtOp => LT
                        | tigerabs.LeOp => LE
                        | tigerabs.GtOp => GT
                        | tigerabs.GeOp => GE
                        | _ => raise Fail "error interno en tigertrans.binOpStrExp"
    in
        Cx (fn (l1,l2) => seq[EXP(externalCall("_stringCompare", [unEx left, unEx right])),
                              MOVE(TEMP t, TEMP tigerframe.rv),
                              CJUMP(oper', TEMP t, CONST 0, l1, l2)])
    end

end
