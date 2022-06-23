structure tigercontrolflow :> tigercontrolflow = struct

    open tigerassem
    open tigergraph
    open tigertemp
    open tigersugar

    infix 0 && ||
    infix 1 << union diff in_ notIn inter equal

    datatype flowgraph = FGRAPH of {control: graph,
                                    def: (node, temp Splayset.set) Splaymap.dict,
                                    use: (node, temp Splayset.set) Splaymap.dict,
                                    ismove: (node, bool) Splaymap.dict}


    fun instrs2graph instrs = let
            val control = newGraph()
            (* carga nodos y aristas, excepto las aristas correspondientes a los saltos *)
            fun cargar [] res = res
              | cargar (x::xs) (def, use, ismove, ant, saltos, labels, temps) = let
                    val n = newNode control
                    val (def', use', ismove', ant', saltos', labels', temps') = case x of
                        AOPER{assem, dst, src, jump} => let
                                val def' = set(def, n, tempSet(dst))
                                val use' = set(use, n, tempSet(src))
                                val ismove' = set(ismove, n, false)
                                (* si es un salto entonces no tengo que crear la arista a la instrucción siguiente en la lista,
                                   pero tengo que guardar (nodo,label) para luego crear la arista correspondiente a la instrucción
                                   a la que se salta, y si no es un salto guardo el nodo anterior para crear la arista que conecta
                                   a la instrucción siguiente *)
                                val labels' = labels
                                val (ant',saltos') = case jump of
                                                         SOME labs => (NONE, (map (fn l => (n,l)) labs)@saltos)
                                                       | NONE => (SOME n,saltos)
                                val temps' = temps union tempSet(dst) union tempSet(src)
                            in (def', use', ismove', ant', saltos', labels', temps') end
                      | ALABEL{assem, lab} => let
                                val def' = set(def, n, emptyTempSet())
                                val use' = set(use, n, emptyTempSet())
                                val ismove' = set(ismove, n, false)
                                val labels' = set(labels, lab, n)
                                val ant' = SOME n
                                val saltos' = saltos
                                val temps' = temps
                            in (def', use', ismove', ant', saltos', labels', temps') end
                      | AMOVE{assem, dst, src} => let
                                val def' = set(def, n, tempSet([dst]))
                                val use' = set(use, n, tempSet([src]))
                                val ismove' = set(ismove, n, true)
                                val labels' = labels
                                val ant' = SOME n
                                val saltos' = saltos
                                val temps' = temps union tempSet([dst,src])
                            in (def', use', ismove', ant', saltos', labels', temps') end
                    val _ = case ant of
                                SOME n' => mk_edge{from=n', to=n}
                                | NONE => ()
                in
                    cargar xs (def', use', ismove', ant', saltos', labels', temps')
                end
            val (def, use, ismove, ant, saltos, labels, temps) = cargar instrs (emptyNodeMap(), emptyNodeMap(), emptyNodeMap(), NONE, [], Splaymap.mkDict String.compare, emptyTempSet())
            (* ahora sí, creo aristas correspondientes a los saltos *)
            val _ = map (fn (n,l) => mk_edge{from=n, to=get(labels, l)}) saltos
        in
            (FGRAPH{control=control, def=def, use=use, ismove=ismove}, temps)
        end

end
