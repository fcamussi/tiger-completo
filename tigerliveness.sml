structure tigerliveness :> tigerliveness = struct

    open tigertemp
    open tigercontrolflow
    open tigersugar

    infix 0 && ||
    infix 1 << union diff in_ notIn inter equal


(* Algoritmo para el cálculo de liveness (pag. 214)
    for each n
        livein[n] <- {}; liveout[n] <- {}
    repeat
        for each n
            livein'[n] <- livein[n]; liveout'[n] <- liveout[n]
            livein[n] <- use[n] U (liveout[n] \ def[n])
            liveout[n] <- U(livein[s]) for each s in succ[n]
    until livein'[n] == livein[n] and liveout'[n] == liveout[n] for all n *)


    fun liveness (FGRAPH{control,def,use,ismove}) = let
            val nodes = nodeSet(tigergraph.nodes control)
            val r_livein = ref (Splayset.foldr (fn (n,y) => set(y, n, emptyTempSet())) (emptyNodeMap()) nodes)
            val r_liveout = ref (Splayset.foldr (fn (n,y) => set(y, n, emptyTempSet())) (emptyNodeMap()) nodes)
            val r_livein' = ref (emptyNodeMap())
            val r_liveout' = ref (emptyNodeMap())

            fun iguales(live,live') = Splayset.foldr (fn (x,y) => get(live,x) equal get(live',x) && y) true nodes

            fun iterar() = let
                in
                    nodes << (fn n => (
                        r_livein' := set(!r_livein', n, get(!r_livein, n));
                        r_liveout' := set(!r_liveout', n, get(!r_liveout, n));
                        r_livein := set(!r_livein, n, get(use, n) union (get(!r_liveout, n) diff get(def, n)));
                        r_liveout := set(!r_liveout, n, foldr (fn(s,y) => get(!r_livein, s) union y) (emptyTempSet()) (tigergraph.succ n))
                    ));
                    if iguales(!r_livein,!r_livein') && iguales(!r_liveout,!r_liveout') then (!r_livein,!r_liveout) else iterar()
                end
        in
            #2 (iterar())
        end

    fun printLive live = let
            fun printTemp x y = Splayset.app (fn x => print (x ^ "; ")) y
        in
            Splaymap.app (fn (x,y) => ((print ((tigergraph.nodename x) ^ " <-> ")); printTemp x y; print "\n")) live;
            print "\n\n"
        end

end

