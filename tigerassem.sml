structure tigerassem = struct

open tigertemp

type reg = string

datatype instr = AOPER of {assem: string, dst: temp list, src: temp list, jump: label list option}
               | ALABEL of {assem: string, lab: label}
               | AMOVE of {assem: string, dst: temp, src: temp}

fun format saytemp =
let
    fun ifmt assem = if assem <> "" then "\t" ^ assem ^ "\n" else ""
    fun lfmt assem = assem ^ "\n"
    fun speak(assem,dst,src,jump) =
    let
        val saylab = (fn x => makeString x)
        fun f(#"'":: #"s":: #"h":: i:: rest) = (explode(String.substring(saytemp(List.nth(src,ord i - ord #"0")), 0, 1)) @ f rest)
          | f(#"'":: #"s":: #"l":: i:: rest) = (explode(String.substring(saytemp(List.nth(src,ord i - ord #"0")), 1, 1)) @ f rest)
          | f(#"'":: #"s":: i:: rest) = (explode(saytemp(List.nth(src,ord i - ord #"0"))) @ f rest)
          | f(#"'":: #"d":: #"h":: i:: rest) = (explode(String.substring(saytemp(List.nth(dst,ord i - ord #"0")), 0, 1)) @ f rest)
          | f(#"'":: #"d":: #"l":: i:: rest) = (explode(String.substring(saytemp(List.nth(dst,ord i - ord #"0")), 1, 1)) @ f rest)
          | f(#"'":: #"d":: i:: rest) = (explode(saytemp(List.nth(dst,ord i - ord #"0"))) @ f rest)
          | f(#"'":: #"j":: i:: rest) = (explode(saylab(List.nth(jump,ord i - ord #"0"))) @ f rest)
          | f(#"'":: #"'":: rest) = #"'" :: f rest
          | f(#"'":: _ :: rest) = raise Fail "bad Assem format"
          | f(c :: rest) = (c :: f rest)
          | f nil = nil
        in implode(f(explode assem))
        end
    in fn AOPER{assem,dst,src,jump=NONE} => speak(ifmt assem,dst,src,nil)
        | AOPER{assem,dst,src,jump=SOME j} => speak(ifmt assem,dst,src,j)
        | ALABEL{assem,...} => lfmt assem
        | AMOVE{assem,dst,src} => speak(ifmt assem,[dst],[src],nil)
    end

fun toString i = if i < 0 then "-" ^ (Int.toString (Int.abs i)) else Int.toString i

end

