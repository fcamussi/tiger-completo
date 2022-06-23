(*
	Frames para el 80386 (sin displays ni registers).

		|    argn    |	fp+4*(n+1)
		|    ...     |
		|    arg2    |	fp+16
		|    arg1    |	fp+12
		|	fp level |  fp+8
		|  retorno   |	fp+4
		|   fp ant   |	fp
		--------------	fp
		|   local1   |	fp-4
		|   local2   |	fp-8
		|    ...     |
		|   localn   |	fp-4*n
*)

structure tigerframe :> tigerframe = struct

open tigertree
open tigerassem

type level = int

val fp = "ix"				(* frame pointer *)
val sp = "sp"				(* stack pointer *)
val rv = "hl"				(* return value  *)
val ov = "??"				(* overflow value (edx en el 386) *)
val wSz = 2					(* word size in bytes *)
(*val log2WSz = 2*)			(* base two logarithm of word size in bytes *)
(*val fpPrev = 0*)			(* offset (bytes) *)
val fpPrevLev = 2*wSz		(* offset (bytes) *)
val argsInicial = 0			(* words *)
val argsOffInicial = 2*wSz	(* bytes *)
val argsGap = wSz			(* bytes *)
(*val regInicial = 0*)		(* reg *)
val localsInicial = 0		(* words *)
val localsGap = ~wSz		(* bytes *)
val localsOffInicial = ~wSz	(* bytes *)
val specialregs = [fp, sp]
(*val argregs = []*)
val callersaves = [rv, "bc", "de"]
val calleesaves = []
val calldefs = callersaves
val maxInt = 32767 (* 2^15 - 1 *)

type register = string

datatype access = InFrame of int | InReg of tigertemp.label

type frame = {
	name: string,
	formals: bool list,
	actualArg: int ref,
	actualLocal: int ref,
    accessArg: (access list) ref
}

datatype frag = PROC of {body: tigertree.stm, frame: frame}
	| STRING of tigertemp.label * string

fun allocArg (f: frame) b = let (* ignoro b, siempre paso los args por pila *)
        val acc = InFrame(!(#actualArg f) * argsGap + argsOffInicial)
        val _ = #actualArg f := !(#actualArg f) + 1
        val _ = #accessArg f := (!(#accessArg f))@[acc]
    in
        acc
    end

fun newFrame{name, formals} = let
        val frame = {
	        name=name,
	        formals=formals,
	        actualArg=ref argsInicial,
	        actualLocal=ref localsInicial,
            accessArg=ref []}
        val _ = allocArg frame true (* static link *)
        val _ = map (fn b => allocArg frame b) (#formals frame) (* argumentos *)
    in frame end

fun name(f: frame) = #name f

fun string(l, s) = let
        fun stringLen s =
	        let	fun aux[] = 0
		        | aux(#"\\":: #"x"::_::_::t) = 2+aux(t)
		        | aux(_::t) = 1+aux(t)
	        in	aux(explode s) end
        fun line [] l = (rev l,[],false)
          | line (#"\\"::(#"x"::(#"0"::(#"a"::r)))) l = (rev l,r,true)
          | line (c::r) l = line r (c::l)
        fun newLines s = let
                val s' = explode s
                val (l,r,n) = line s' []
            in
                (if l <> [] then "\t.ascii \"" ^ implode l ^ "\"\n" else "") ^ 
                (if n = true then "\t.db 0x0A\n\t.db 0x0D\n" else "") ^
                (if r <> [] then newLines (implode r) else "")
            end
    in
        l ^ ":\n\t.dw " ^ Int.toString (stringLen s) ^ "\n" ^ newLines(tigertemp.makeString(s))
    end

fun formals(f: frame) = !(#accessArg f)

(*fun maxRegFrame(f: frame) = !(#actualReg f)*)

fun allocLocal (f: frame) b = 
    case b of
        true => let val ret = InFrame(!(#actualLocal f) * localsGap + localsOffInicial)
                    val _ = #actualLocal f := !(#actualLocal f) + 1
                in ret end
        | false => InReg(tigertemp.newtemp())

fun exp(InFrame k) e = MEM(BINOP(PLUS, TEMP(fp), CONST k))
| exp(InReg l) e = TEMP l

fun externalCall(s, l) = CALL(NAME s, l)

fun procEntryExit1 (body,frame) = let
        fun seq [] = EXP (CONST 0)
	        | seq [s] = s
	        | seq (x::xs) = SEQ (x, seq xs)
        (* muevo calleesaves a temporarios antes del body y restauro después para que el spiller
           se encargue de guardarlos/recuperarlos en/a memoria si es necesario usarlos (pag. 261) *)
        val temps = List.tabulate (length calleesaves, fn x => tigertemp.newtemp())
        val save = map MOVE (ListPair.zip (map TEMP temps, map TEMP calleesaves))
        val fetch = map MOVE (ListPair.zip (map TEMP calleesaves, map TEMP temps))
    in
        seq(save@[body]@fetch)
    end

(* Al agregar estos registros al final de la función como src estamos obligando a que se mantengan
   vivos (interfiriendo todos los temporarios), y por tanto que en el coloreo no se usen los registros
   especiales, y en el caso de los calleesaves que se spilleen a la memoria ya que procEntryExit1
   los mueve a temporales (pag. 208) *)
fun procEntryExit2 (instrs,frame) = instrs@[AOPER{assem = "",
                                                  dst = [],
                                                  src = specialregs@calleesaves,
                                                  jump = SOME []}]

(* Prólogo y epílogo *)
fun procEntryExit3 (instrs,frame) = let
        val offset = !(#actualLocal frame) * wSz (* lugar para los locales incluyendo los registros que se spillean *)
        val prolog = (*".globl " ^ name frame ^ "\n.type " ^ name frame ^ ", @function\n" ^ (name frame) ^ ":\n" ^*)
                     ".globl " ^ name frame ^ "\n" ^
                     name frame ^ ":\n" ^
                     "\tpush " ^ fp ^ "\n" ^
                     "\tld " ^ fp ^ ", #0\n" ^
                     "\tadd " ^ fp ^ ", " ^ sp ^ "\n" ^
                     "\tld iy, #" ^ toString (~offset) ^ "\n" ^
                     "\tadd iy, sp\n" ^
                     "\tld sp, iy\n"
        val epilog = "\tld " ^ sp ^ ", " ^ fp ^ "\n" ^
                     "\tpop " ^ fp ^ "\n" ^
                     "\tret\n"
    in
        {prolog=prolog,body=instrs,epilog=epilog}
    end


(*                     "\txorl " ^ rv ^ ", %" ^ rv ^ "\n" ^ (* se supone que es más rapido que hacer movl 0, %rv *)*)

end
