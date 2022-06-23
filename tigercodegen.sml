structure tigercodegen :> tigercodegen = struct

open tigertree
open tigerassem
open tigerframe
open tigertemp


fun codegen frame stm =
let
    val ilist = ref (nil: instr list)

    fun emit x = ilist := x :: !ilist

    fun result(gen) = let val t = newtemp() in gen t; t end

    fun munchStm(SEQ(a,b)) = (munchStm a; munchStm b)
      | munchStm(MOVE(MEM(BINOP(PLUS, e1, CONST i)), e2)) =
            emit(AOPER{assem = "1- movl %'s1, " ^ toString i ^ "(%'s0)",
                       src = [munchExp e1, munchExp e2],
                       dst = [],
                       jump = NONE})
      | munchStm(MOVE(MEM(BINOP(PLUS, CONST i, e1)), e2)) =
            emit(AOPER{assem = "2- movl %'s1, " ^ toString i ^ "(%'s0)",
                       src = [munchExp e1, munchExp e2],
                       dst = [],
                       jump = NONE})
      | munchStm(MOVE(MEM(CONST i), e2)) =
            emit(AOPER{assem = "3- movl %'s0, (" ^ toString i ^ ")",
                       src = [munchExp e2],
                       dst = [],
                       jump = NONE})
      | munchStm(MOVE(MEM(e1), e2)) =
            emit(AOPER{assem = "4- movl %'s1, (%'s0)",
                       src = [munchExp e1, munchExp e2],
                       dst = [],
                       jump = NONE})
      | munchStm(MOVE(TEMP i, e2)) =
            emit(AMOVE{assem = "ld 'dh0, 'sh0\n\t" ^
                               "ld 'dl0, 'sl0",
                       src = munchExp e2,
                       dst = i})
      | munchStm(LABEL lab) =
            emit(ALABEL{assem = lab ^ ":",
                        lab = lab})
      | munchStm(JUMP (NAME lab, _)) =
            emit(AOPER{assem = "jp 'j0",
                       src = [],
                       dst = [],
                       jump = SOME [lab]})
      | munchStm (CJUMP (relop, e1, e2, l1, l2)) = let
            val (cmp,jmp) = case relop of
                                EQ => ("or a\n\tsbc 'd0, 's1", "jp z, 'j0") (* or a es para que se ponga la carry flag en 0 *)
                              | NE => ("or a\n\tsbc 'd0, 's1", "jp nz, 'j0")
                              | LT => ("or a\n\tsbc 'd0, 's1", "jp c, 'j0")
                              | GT => ("scf\n\tsbc 'd0, 's1", "jp nc, 'j0") (* scf pone la carry flag en 1 *)
                              | LE => ("scf\n\tsbc 'd0, 's1", "jp c, 'j0")
                              | GE => ("or a\n\tsbc 'd0, 's1", "jp nc, 'j0")
                              | _ => raise Fail "error interno en tigercodegen.munchStm (1)"
        in
            emit(AMOVE{assem = "ld 'dh0, 'sh0\n\t" ^
                               "ld 'dl0, 'sl0",
                       src = munchExp e1,
                       dst = rv});
            emit(AOPER{assem = cmp,
                        src = [rv, munchExp e2],
                        dst = [rv],
                        jump = NONE});
            emit(AOPER{assem = jmp,
                        src = [],
                        dst = [],
                        jump = SOME [l1,l2]})
        end
      | munchStm(EXP e) = (munchExp e; ())
      | munchStm stm = raise Fail ("error interno en tigercodegen.munchStm (2):" ^ (tigerit.tree stm))
    and munchExp(CALL(NAME lab, args)) = let
            val _ = munchArgs(rev args)
            val numargs = length args
            fun repeat 0 str = ""
              | repeat n str = str ^ (repeat (n - 1) str)
        in
            result(fn r => (emit(AOPER{assem = "call " ^ lab,
                                       src = [], (* Nada, paso todo por stack *)
                                       dst = calldefs, (* Hace que se spilleen los callersaves si se van a usar en la función (pag 205 y 237 *)
                                       jump = NONE});
                            if numargs > 0 then (* por cada push tenemos que sumar wSz al sp *)
                                emit(AOPER{assem = repeat numargs "pop af\n",
                                           src = [],
                                           dst = [],
                                           jump = NONE})
                            else ()))
        end
      | munchExp(MEM(BINOP(PLUS, e1, CONST i))) =
            result(fn r => emit(AOPER{assem = "6- movl " ^ toString i ^ "(%'s0), %'d0",
                                      src = [munchExp e1],
                                      dst = [r],
                                      jump = NONE}))
      | munchExp(MEM(BINOP(PLUS, CONST i, e1))) =
            result(fn r => emit(AOPER{assem = "ld 'dh0, " ^ toString i ^ "('s0)\n\t" ^
                                              "ld 'dl0, " ^ toString (i + 1) ^ "('s0)",
                                      src = [munchExp e1],
                                      dst = [r],
                                      jump = NONE}))
      | munchExp(MEM(CONST i)) =
            result(fn r => emit(AOPER{assem = "8- movl (" ^ toString i ^ "), %'d0",
                                      src = [],
                                      dst = [r],
                                      jump = NONE}))
      | munchExp(MEM(e1)) =
            result(fn r => emit(AOPER{assem = "9- movl (%'s0), %'d0",
                                      src = [munchExp e1],
                                      dst = [r],
                                      jump = NONE}))
      | munchExp(CONST i) =
            result(fn r => emit(AOPER{assem = "ld 'd0, #" ^ toString i,
                                      src = [],
                                      dst = [r],
                                      jump = NONE}))
      | munchExp(BINOP(PLUS, e1, CONST 1)) =
            result(fn r => (emit(AMOVE{assem = "ld 'dh0, 'sh0\n\t" ^
                                               "ld 'dl0, 'sl0",
                                       src = munchExp e1,
                                       dst = r});
                            emit(AOPER{assem = "inc 'd0",
                                      src = [r],
                                      dst = [r],
                                      jump = NONE})))
      | munchExp(BINOP(PLUS, CONST 1, e1)) =
            result(fn r => (emit(AMOVE{assem = "ld 'dh0, 'sh0\n\t" ^
                                               "ld 'dl0, 'sl0",
                                       src = munchExp e1,
                                       dst = r});
                            emit(AOPER{assem = "inc 'd0",
                                      src = [r],
                                      dst = [r],
                                      jump = NONE})))
      | munchExp(BINOP(PLUS, e1, CONST i)) =
            result(fn r => (emit(AOPER{assem = "ld 'd0, #" ^ toString i,
                                       src = [],
                                       dst = [rv],
                                       jump = NONE});
                            emit(AOPER{assem = "add 'd0, 's1",
                                      src = [rv, munchExp e1],
                                      dst = [rv],
                                      jump = NONE});
                            emit(AMOVE{assem = "ld 'dh0, 'sh0\n\t" ^
                                               "ld 'dl0, 'sl0",
                                       src = rv,
                                       dst = r})))
      | munchExp(BINOP(PLUS, CONST i, e1)) =
            result(fn r => (emit(AOPER{assem = "ld 'd0, #" ^ toString i,
                                       src = [],
                                       dst = [rv],
                                       jump = NONE});
                            emit(AOPER{assem = "add 'd0, 's1",
                                      src = [rv, munchExp e1],
                                      dst = [rv],
                                      jump = NONE});
                            emit(AMOVE{assem = "ld 'dh0, 'sh0\n\t" ^
                                               "ld 'dl0, 'sl0",
                                       src = rv,
                                       dst = r})))
      | munchExp(BINOP(PLUS, e1, e2)) =
            result(fn r => (emit(AMOVE{assem = "ld 'dh0, 'sh0\n\t" ^
                                               "ld 'dl0, 'sl0",
                                       src = munchExp e1,
                                       dst = rv});
                            emit(AOPER{assem = "add 'd0, 's1",
                                       src = [rv, munchExp e2],
                                       dst = [rv],
                                       jump = NONE});
                            emit(AMOVE{assem = "ld 'dh0, 'sh0\n\t" ^
                                               "ld 'dl0, 'sl0",
                                       src = rv,
                                       dst = r})))
      | munchExp(BINOP(MINUS, CONST 0, e1)) =
            result(fn r => (emit(AMOVE{assem = "15- movl %'s0, %'d0",
                                       src = munchExp e1,
                                       dst = r});
                            emit(AOPER{assem = "16- negl %'d0",
                                      src = [r],
                                      dst = [r],
                                      jump = NONE})))
      | munchExp(BINOP(MINUS, e1, CONST i)) =
            result(fn r => (emit(AMOVE{assem = "17- movl %'s0, %'d0",
                                       src = munchExp e1,
                                       dst = r});
                            emit(AOPER{assem = "18- subl $" ^ toString i ^ ", %'d0",
                                      src = [r],
                                      dst = [r],
                                      jump = NONE})))
      | munchExp(BINOP(MINUS, CONST i, e1)) =
            result(fn r => (emit(AOPER{assem = "19- movl $" ^ toString i ^ ", %'d0",
                                       src = [],
                                       dst = [r],
                                       jump = NONE});
                            emit(AOPER{assem = "20- subl %'s1, %'d0",
                                      src = [r, munchExp e1],
                                      dst = [r],
                                      jump = NONE})))
      | munchExp(BINOP(MINUS, e1, e2)) =
            result(fn r => (emit(AMOVE{assem = "21- movl %'s0, %'d0",
                                       src = munchExp e1,
                                       dst = r});
                            emit(AOPER{assem = "22- subl %'s1, %'d0",
                                       src = [r, munchExp e2],
                                       dst = [r],
                                       jump = NONE})))
      | munchExp(TEMP t) = makeString t
      | munchExp(NAME l) = 
            result(fn r => emit(AOPER{assem = "ld 'd0, #" ^ makeString l,
                                      src = [],
                                      dst = [r],
                                      jump = NONE}))
      | munchExp exp = raise Fail ("error interno en tigercodegen.munchExp:" ^ (tigerit.tree (EXP exp)))
    and munchArgs([]) = []
      | munchArgs(x::xs) =
            let val _ = emit(AOPER{assem = "push 's0",
                                   dst = [],
                                   src = [munchExp x],
                                   jump = NONE})
            in munchArgs(xs) end
in munchStm stm;
   rev(!ilist)
end

fun codegenerator(stms,frame) = foldr (fn (x,y) => (codegen frame x)@y) [] stms

end
