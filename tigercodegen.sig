signature tigercodegen = sig

val codegen : tigerframe.frame -> tigertree.stm -> tigerassem.instr list
val codegenerator : tigertree.stm list * tigerframe.frame -> tigerassem.instr list

end

