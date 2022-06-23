open tigerlex
open tigergrm
open tigerescap
open tigerseman
open tigercodegen
open tigerassem
open BasicIO Nonstdio

fun lexstream(is: instream) =
	Lexing.createLexer(fn b => fn n => buff_input is b 0 n);

fun errParsing(lbuf) = (print("Error en parsing!("
	^(makestring(!num_linea))^
	")["^(Lexing.getLexeme lbuf)^"]\n"); raise Fail "fin!")

fun main(args) = let
	    fun arg(l, s) = (List.exists (fn x => x=s) l, List.filter (fn x => x<>s) l)

        (* construye el ejecutable *)
        fun makeExec source filename = let
                val filename = Path.file (Path.base filename)
                val filename_asm = filename ^ ".asm"
                val out = TextIO.openOut filename_asm
                val tigerpath = Path.dir(List.nth(Mosml.argv(), 0)) (* el dir donde está el tiger *)
            in
                TextIO.output (out, source);
                TextIO.closeOut out;
                Process.system ("sdasz80 -l -o \"" ^ filename_asm ^ "\"");
                Process.system ("sdcc -mz80 --code-loc 0x0106 --data-loc 0 --no-std-crt0 \"" ^ filename ^ ".rel\"" ^
                                " \"" ^ tigerpath ^ "\"/crt0.rel" ^
                                " \"" ^ tigerpath ^ "\"/runtime.rel" ^
                                " -o \"" ^ filename ^ ".ihx\"");
                Process.system ("\"" ^ tigerpath ^ "\"/hex2bin \"" ^ filename ^ ".ihx\"");
                Process.system ("mv \"" ^ filename ^ ".bin\"" ^ " \"" ^ filename ^ ".com\"")
            end

        (* genera el código ensamblador *)
        fun makeSource procs strings = let
                fun makeText strings = foldr (fn (x,y) => (tigerframe.string x) ^ y) "" strings
                fun asm saytemp (instrs,frame) = let
                        val {prolog,body,epilog} = tigerframe.procEntryExit3(instrs,frame)
                    in
                        prolog ^ (foldr (fn (x,y) => (format saytemp x) ^ y) "" body) ^ epilog
                    end
            in
                "\t.globl _print\n" ^
                "\t.globl _stringCompare\n\n" ^
                "\t.area _DATA\n" ^ (makeText strings) ^ "\n" ^ "\t.area _DATA\n" ^
                (foldr (fn (((instrs,alloc),frame),y) => (asm (fn x => Splaymap.find(alloc,x)) (instrs,frame))^y) "" procs)
            end

		val (arbol, l1)		= arg(args, "-arbol")
		val (escapes, l2)	= arg(l1, "-escapes") 
		val (ir, l3)		= arg(l2, "-ir") 
		val (canon, l4)		= arg(l3, "-canon") 
		val (code, l5)		= arg(l4, "-code") 
		val (flow, l6)		= arg(l5, "-flow") 
		val (inter, l7)		= arg(l6, "-inter") 
		val entrada =
			case l7 of
			[n] => let val d = open_in n handle _ => raise Fail (n^" no existe!")
                   in if (Path.ext n) <> SOME "tig"
                        then raise Fail (n^" no es un archivo tiger!")
                        else d
                   end
			| [] => std_in
			| _ => raise Fail "opcio'n dsconocida!"

        (* análisis léxico, parseo, y cálculo de escapes *)
		val lexbuf = lexstream entrada
		val expr = prog Tok lexbuf handle _ => errParsing lexbuf
		val _ = findEscape(expr)
		val _ = if arbol then tigerpp.exprAst expr else ()

        (* análisis semántico y traducción a código intermedio *)
		val _ = transProg(expr);
        val frags = tigertrans.getResult()
        val _ = if ir then print(tigertrans.Ir(frags)) else ()

        (* canonicalización *)
    	val (procs,strings) = tigercanon.canonicalizate frags
        val _ = if canon then tigercanon.printcanon(procs,strings) else ()

        (* selección de instrucciones *)
        val procs' = foldr (fn ((stms,frame),y) => (codegenerator(stms,frame),frame)::y) [] procs

        (* coloreo *)
        val registers = tigerframe.specialregs@tigerframe.callersaves@tigerframe.calleesaves
        val procs'' = foldr (fn ((instrs,frame),y) => (tigercolor.color(tigerframe.procEntryExit2(instrs,frame),registers,frame),frame)::y) [] procs'

        (* ensamblado y linkeo *)
        val _ = makeExec (makeSource procs'' strings) (hd l7)

	in
		print "yes!!\n"
	end	handle Fail s => print("Fail: "^s^"\n")

val _ = main(CommandLine.arguments())
