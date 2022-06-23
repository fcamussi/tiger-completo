signature tigercolor = sig

    type allocation = (tigertemp.temp, tigerframe.register) Splaymap.dict

    val color: tigerassem.instr list * tigerframe.register list * tigerframe.frame -> tigerassem.instr list * allocation
    val printColors : allocation -> unit

end
