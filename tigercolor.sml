structure tigercolor :> tigercolor = struct

    open tigersugar
    open tigerspill

    infix 0 && ||
    infix 1 << union diff in_ notIn inter equal

    type allocation = (tigertemp.temp, tigerframe.register) Splaymap.dict



    fun printColors alloc = print(Splaymap.foldr (fn (x,y,z) => (x ^ " <-> " ^ y ^ "\n") ^ z) "\n" alloc)


    fun emptyEdgeSet() = Splayset.empty (fn ((u1,v1),(u2,v2)) => case tigertemp.compare(u1,u2) of
                                                                     EQUAL => tigertemp.compare(v1,v2) |
                                                                     cmp => cmp)


    fun color(instrs, registers, frame) = let

        (* Colores *)
        val r_precolored = ref (tempSet(registers))
        val K = card(!r_precolored)

        (* Instrucciones *)
        val r_instrs = ref instrs

        (* Resultado del análisis de liveness *)
        val r_nodes = ref []
        val r_def = ref (emptyNodeMap())
        val r_use = ref (emptyNodeMap())
        val r_ismove = ref (emptyNodeMap())
        val r_live = ref (emptyNodeMap())
        val r_temps = ref (emptyTempSet())

        (* Estructuras para el coloreo *)
        val r_moveList = ref (emptyTempMap())
        val r_workListMoves = ref (emptyNodeSet())
        val r_adjSet = ref (emptyEdgeSet())
        val r_adjList = ref (emptyTempMap())
        val r_degree = ref (emptyTempMap())
        val r_initial = ref (emptyTempSet())
        val r_spillWorkList = ref (emptyTempSet())
        val r_freezeWorkList = ref (emptyTempSet())
        val r_simplifyWorkList = ref (emptyTempSet())
        val r_activeMoves = ref (emptyNodeSet())
        val r_selectStack = ref (emptyStack())
        val r_coalescedNodes = ref (emptyTempSet())
        val r_coalescedMoves = ref (emptyNodeSet())
        val r_constrainedMoves = ref (emptyNodeSet())
        val r_alias = ref (emptyTempMap())
        val r_frozenMoves = ref (emptyNodeSet())
        val r_coloredNodes = ref (emptyTempSet())
        val r_spilledNodes = ref (emptyTempSet())
        val r_color = ref (Splayset.foldr (fn (x,y) => set(y,x,x)) (emptyTempMap()) (!r_precolored))

        fun getNodeSet(m,x) = case peek(m,x) of SOME y => y | NONE => emptyNodeSet()

        fun getTempSet(m,x) = case peek(m,x) of SOME y => y | NONE => emptyTempSet()

        fun getDegree(m,x) = let
                val infinito = valOf(Int.maxInt)
            in
                if x in_ !r_precolored then infinito else case peek(m,x) of SOME y => y | NONE => 0
            end

        fun livenessAnalysis() = let
                val (fgraph as tigercontrolflow.FGRAPH{control,def,use,ismove},temps) = tigercontrolflow.instrs2graph (!r_instrs)
            in
                r_nodes := tigergraph.nodes control;
                r_def := def;
                r_use := use;
                r_ismove := ismove;
                r_live := tigerliveness.liveness fgraph;
                r_temps := temps
            end

        fun addEdge(u,v) = let
            in
                if (u,v) notIn !r_adjSet && u <> v then (
                    r_adjSet := addList(!r_adjSet,[(u,v),(v,u)]);
                    if u notIn !r_precolored then (
                        r_adjList := set(!r_adjList,u,add(getTempSet(!r_adjList,u),v));
                        r_degree := set(!r_degree,u,getDegree(!r_degree,u)+1)
                    ) else ();
                    if v notIn !r_precolored then (
                        r_adjList := set(!r_adjList,v,add(getTempSet(!r_adjList,v),u));
                        r_degree := set(!r_degree,v,getDegree(!r_degree,v)+1)
                    ) else ()
                ) else ()
            end

        fun build() = let
            in
                nodeSet(rev (!r_nodes)) << (fn i => (
                    if get(!r_ismove,i) then (
                        r_live := set(!r_live,i,getTempSet(!r_live,i) diff getTempSet(!r_use,i));
                        (getTempSet(!r_def,i) union getTempSet(!r_use,i)) << (fn n => (
                            r_moveList := set(!r_moveList,n,add(getNodeSet(!r_moveList,n),i))
                        ));
                        r_workListMoves := add(!r_workListMoves, i)
                    ) else ();
                    r_live := set(!r_live,i,getTempSet(!r_live,i) union getTempSet(!r_def,i));
                    getTempSet(!r_def,i) << (fn d => (
                        getTempSet(!r_live,i) << (fn l => (
                            addEdge(l,d)
                        ))
                    ));
                    r_live := set(!r_live,i,getTempSet(!r_use,i) union (getTempSet(!r_live,i) diff getTempSet(!r_def,i)))
                ))
            end

        fun nodeMoves(n) = getNodeSet(!r_moveList,n) inter (!r_activeMoves union !r_workListMoves)

        fun moveRelated(n) = isNotEmpty(nodeMoves(n))

        fun makeWorkList() = let
            in
                !r_initial << (fn n => (
                    r_initial := delete(!r_initial,n);
                    if getDegree(!r_degree,n) >= K then (
                        r_spillWorkList := add(!r_spillWorkList,n)
                    ) else if moveRelated(n) then (
                        r_freezeWorkList := add(!r_freezeWorkList,n)
                    ) else (
                        r_simplifyWorkList := add(!r_simplifyWorkList,n)
                    )
                ))
            end

        fun adjacent(n) = getTempSet(!r_adjList,n) diff (tempSet(!r_selectStack) union !r_coalescedNodes)

        fun enableMoves(nodes) = let
            in
                nodes << (fn n => (
                    nodeMoves(n) << (fn m => (
                        if m in_ !r_activeMoves then (
                            r_activeMoves := delete(!r_activeMoves,m);
                            r_workListMoves := add(!r_workListMoves,m)
                        ) else ()
                    ))
                ))
            end

        fun decrementDegree(m) = let
                val d = getDegree(!r_degree,m)
            in
                r_degree := set(!r_degree,m,d-1);
                if d = K then (
                    enableMoves(add(adjacent(m),m));
                    r_spillWorkList := delete(!r_spillWorkList,m);
                    if moveRelated(m) then (
                        r_freezeWorkList := add(!r_freezeWorkList,m)
                    ) else (
                        r_simplifyWorkList := add(!r_simplifyWorkList,m)
                    )
                ) else ()
            end

        fun simplify() = let
                val n = takeOne(!r_simplifyWorkList)
            in
                r_simplifyWorkList := delete(!r_simplifyWorkList,n);
                r_selectStack := push(!r_selectStack,n);
                adjacent(n) << (fn m => (
                    decrementDegree(m)
                ))
            end

        fun getAlias(n) = if n in_ !r_coalescedNodes then getAlias(get(!r_alias,n)) else n

        fun addWorkList(u) = let
            in
                if u notIn !r_precolored && not (moveRelated(u)) && getDegree(!r_degree,u) < K then (
                    r_freezeWorkList := delete(!r_freezeWorkList,u);
                    r_simplifyWorkList := add(!r_simplifyWorkList,u)
                ) else ()
            end

        fun oK(t,r) = getDegree(!r_degree,t) < K || t in_ !r_precolored || (t,r) in_ !r_adjSet

        fun conservative(nodes) = let
                val r_k = ref 0
            in
                nodes << (fn n => (
                    if getDegree(!r_degree,n) >= K then r_k := !r_k+1 else ()
                ));
                !r_k < K
            end

        fun combine(u,v) = let
            in
                if v in_ !r_freezeWorkList then (
                    r_freezeWorkList := delete(!r_freezeWorkList,v)
                ) else (
                    r_spillWorkList := delete(!r_spillWorkList,v)
                );
                r_coalescedNodes := add(!r_coalescedNodes,v);
                r_alias := set(!r_alias,v,u);
                r_moveList := set(!r_moveList,u,getNodeSet(!r_moveList,u) union getNodeSet(!r_moveList,v));
                enableMoves(tempSet([v]));
                adjacent(v) << (fn t => (
                    addEdge(t,u);
                    decrementDegree(t)
                ));
                if getDegree(!r_degree,u) >= K && u in_ !r_freezeWorkList then (
                    r_freezeWorkList := delete(!r_freezeWorkList, u);
                    r_spillWorkList := add(!r_spillWorkList,u)
                ) else ()
            end

        fun coalesce() = let
                val m = takeOne(!r_workListMoves)
                val x = getAlias(takeOne(getTempSet(!r_use,m)))
                val y = getAlias(takeOne(getTempSet(!r_def,m)))
                val (u,v) = if y in_ !r_precolored then (y,x) else (x,y)
            in
                r_workListMoves := delete(!r_workListMoves,m);
                if u = v then (
                    r_coalescedMoves := add(!r_coalescedMoves,m);
                    addWorkList(u)
                ) else if v in_ !r_precolored || (u,v) in_ !r_adjSet then (
                    r_constrainedMoves := add(!r_constrainedMoves,m);
                    addWorkList(u);
                    addWorkList(v)
                ) else if (u in_ !r_precolored && Splayset.foldr (fn (t,y) => oK(t,u) && y) true (adjacent(v))) ||
                          (u notIn !r_precolored && conservative(adjacent(u) union adjacent(v))) then (
                        r_coalescedMoves := add(!r_coalescedMoves,m);
                        combine(u,v);
                        addWorkList(u)
                ) else (
                        r_activeMoves := add(!r_activeMoves,m)
                )
            end

        fun freezeMoves(u) = let
            in
                nodeMoves(u) << (fn m => (
                    let
                        val x = takeOne(getTempSet(!r_use,m))
                        val y = takeOne(getTempSet(!r_def,m))
                        val v = if getAlias(y) = getAlias(u) then getAlias(x) else getAlias(y)
                    in
                        r_activeMoves := delete(!r_activeMoves,m);
                        r_frozenMoves := add(!r_frozenMoves,m);
                        if isEmpty(nodeMoves(v)) && getDegree(!r_degree,v) < K then (
                            r_freezeWorkList := delete(!r_freezeWorkList,v);
                            r_simplifyWorkList := add(!r_simplifyWorkList,v)
                        ) else ()
                    end
                ))
            end

        fun freeze() = let
                val u = takeOne(!r_freezeWorkList)
            in
                r_freezeWorkList := delete(!r_freezeWorkList,u);
                r_simplifyWorkList := add(!r_simplifyWorkList,u);
                freezeMoves(u)
            end

        (* Heurística para seleccionar el temporal a spillear:
           Se elije el temporal con menor cantidad de defs + uses *)
        fun heuristic() = let
                fun get0(m,x) = case peek(m,x) of SOME y => y | NONE => 0

                val r_cost = ref (emptyTempMap())
                val r_min = ref (takeOne(!r_spillWorkList))
            in
                nodeSet(!r_nodes) << (fn i => (
                    (getTempSet(!r_def,i) union getTempSet(!r_use,i)) << (fn n => (
                        r_cost := set(!r_cost,n,get0(!r_cost,n)+1)
                    ))
                ));
                (!r_spillWorkList) << (fn n => (
                    if get(!r_cost,n) < get(!r_cost,!r_min) then (
                        r_min := n
                    ) else ()
                ));
                !r_min
            end

        fun selectSpill() = let
                val m = heuristic()
            in
                r_spillWorkList := delete(!r_spillWorkList,m);
                r_simplifyWorkList := add(!r_simplifyWorkList,m);
                freezeMoves(m)
            end

        fun assignColors() = let
            in
                while isNotEmpty(tempSet(!r_selectStack)) do
                    let
                        val n = top(!r_selectStack)
                        val r_okColors = ref (!r_precolored)
                    in
                        r_selectStack := pop(!r_selectStack);
                        getTempSet(!r_adjList,n) << (fn w => (
                            if getAlias(w) in_ (!r_coloredNodes union !r_precolored) then (
                                r_okColors := delete(!r_okColors,get(!r_color,getAlias(w)))
                            ) else ()
                        ));
                        if isEmpty(!r_okColors) then (
                            r_spilledNodes := add(!r_spilledNodes,n)
                        ) else (
                            r_coloredNodes := add(!r_coloredNodes,n);
                            r_color := set(!r_color,n,takeOne(!r_okColors))
                        )
                    end;
                !r_coalescedNodes << (fn n => (
                    r_color := set(!r_color,n,get(!r_color,getAlias(n)))
                ))
            end

        fun rewriteProgram() = let
                val (instrs,newTemps) = spill(!r_instrs, !r_spilledNodes, frame)
            in
                (*print ("Spill: " ^ (Splayset.foldr (fn (x,y) => x^" "^y) "\n" (!r_spilledNodes)));*)
                r_instrs := instrs
            end

        fun clean() = let
            in
                r_moveList := emptyTempMap();
                r_workListMoves := emptyNodeSet();
                r_adjSet := emptyEdgeSet();
                r_adjList := emptyTempMap();
                r_degree := emptyTempMap();
                r_initial := emptyTempSet();
                r_spillWorkList := emptyTempSet();
                r_freezeWorkList := emptyTempSet();
                r_simplifyWorkList := emptyTempSet();
                r_activeMoves := emptyNodeSet();
                r_selectStack := emptyStack();
                r_coalescedNodes := emptyTempSet();
                r_coalescedMoves := emptyNodeSet();
                r_constrainedMoves := emptyNodeSet();
                r_alias := emptyTempMap();
                r_frozenMoves := emptyNodeSet();
                r_coloredNodes := emptyTempSet();
                r_spilledNodes := emptyTempSet();
                r_color := Splayset.foldr (fn (x,y) => set(y,x,x)) (emptyTempMap()) (!r_precolored)
            end

        fun main() = let
            in
                livenessAnalysis();
                r_initial := (!r_temps diff !r_precolored); (* Nodos a colorear *)
                build();
                makeWorkList();
                while isNotEmpty(!r_simplifyWorkList) || isNotEmpty(!r_workListMoves) ||
                      isNotEmpty(!r_freezeWorkList) || isNotEmpty(!r_spillWorkList) do (
                    if isNotEmpty(!r_simplifyWorkList) then simplify()
                    else if isNotEmpty(!r_workListMoves) then coalesce()
                    else if isNotEmpty(!r_freezeWorkList) then freeze()
                    else if isNotEmpty(!r_spillWorkList) then selectSpill()
                    else ()
                );
                assignColors();
                if isNotEmpty(!r_spilledNodes) then (
                    rewriteProgram();
                    clean();
                    main()
                ) else ()
            end

            (* Elimina moves con el mismo color en el origen y el destino *)
            fun deleteMoves [] = []
              | deleteMoves (x::xs) = case x of
                    tigerassem.AMOVE{assem,dst,src} => if get(!r_color,dst) = get(!r_color,src) then deleteMoves xs else x::(deleteMoves xs)
                  | x => x::(deleteMoves xs)
        in
            main();
            r_instrs := deleteMoves(!r_instrs);
            (!r_instrs,!r_color)
        end

end

