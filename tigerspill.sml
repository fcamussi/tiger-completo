structure tigerspill :> tigerspill = struct

    open tigerassem
    open tigersugar
    open tigerframe

    infix 0 && ||
    infix 1 << union diff in_ notIn inter


    fun store(t,m) = AOPER{assem = "ld " ^ toString m ^ "(" ^ fp ^ "), 'sh0 ; store ;\n\t" ^
                                   "ld " ^ toString (m + 1) ^ "(" ^ fp ^ "), 'sl0 ; spill ;",
                           dst = [],
                           src = [t],
                           jump = NONE}

    fun fetch(t,m) = AOPER{assem = "ld 'dh0, " ^ toString m ^ "(" ^ fp ^ ") ; load  ;\n\t" ^
                                   "ld 'dl0, " ^ toString (m + 1) ^ "(" ^ fp ^ ") ; spill ;",
                           dst = [t],
                           src = [],
                           jump = NONE}

    fun def(i) = case i of AOPER{dst, ...} => tempSet(dst)
                         | AMOVE{dst, ...} => tempSet([dst])
                         | ALABEL{...} => emptyTempSet()

    fun use(i) = case i of AOPER{src, ...} => tempSet(src)
                         | AMOVE{src, ...} => tempSet([src])
                         | ALABEL{...} => emptyTempSet()

    fun spill(instrs, spilledNodes, frame) = let
            val r_instrs = ref instrs
            val r_lastTemp = ref ""
            val r_newTemps = ref (emptyTempSet())

            fun newtemp() = let
                    val t = tigertemp.newtemp()
                in
                    r_newTemps := add(!r_newTemps, t);
                    r_lastTemp := t
                end

            fun replace(xs,x,y) = foldr (fn (x',z) => if x = x' then y::z else x'::z) [] xs

            fun memdir acc = case acc of InFrame m => m
                                       | _ => raise Fail "error interno en tigerspill.spill (1)"

            fun rewrite([], _, _) = []
              | rewrite(i::is, v, m) = let
                    val (i',f) = if v in_ use(i) then (
                        if !r_lastTemp = "" then raise Fail "error interno en tigerspill.spill (2)" else ();
                        ((case i of
                              AOPER{assem, dst, src, jump} => AOPER{assem=assem, dst=dst, src=replace(src,v,!r_lastTemp), jump=jump}
                            | ALABEL x => ALABEL x
                            | AMOVE{assem, dst, src} => AMOVE{assem=assem, dst=dst, src=(!r_lastTemp)}), [fetch(!r_lastTemp,m)])
                    ) else (i,[])
                    val (i'',s) = if v in_ def(i') then (
                        newtemp();
                        ((case i' of
                              AOPER{assem, dst, src, jump} => AOPER{assem=assem, dst=replace(dst,v,!r_lastTemp), src=src, jump=jump}
                            | ALABEL x => ALABEL x
                            | AMOVE{assem, dst, src} => AMOVE{assem=assem, dst=(!r_lastTemp), src=src}), [store(!r_lastTemp,m)])
                    ) else (i',[])
                in
                    f@[i'']@s@rewrite(is, v, m)
                end
        in
            spilledNodes << (fn v => (
                r_lastTemp := ""; (* para chequeo interno *)
                r_instrs := rewrite(!r_instrs, v, memdir(allocLocal frame true))
            ));
            (!r_instrs,!r_newTemps)
        end

end

