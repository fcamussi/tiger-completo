signature tigerliveness = sig

    val liveness : tigercontrolflow.flowgraph -> (tigergraph.node, tigertemp.temp Splayset.set) Splaymap.dict
    val printLive : (tigergraph.node, tigertemp.temp Splayset.set) Splaymap.dict -> unit

end

