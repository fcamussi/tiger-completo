signature tigercontrolflow = sig

    datatype flowgraph = FGRAPH of {control: tigergraph.graph,
                                    def: (tigergraph.node, tigertemp.temp Splayset.set) Splaymap.dict,
                                    use: (tigergraph.node, tigertemp.temp Splayset.set) Splaymap.dict,
                                    ismove: (tigergraph.node, bool) Splaymap.dict}

    val instrs2graph : tigerassem.instr list -> flowgraph * tigertemp.temp Splayset.set

end
