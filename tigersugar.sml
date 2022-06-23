structure tigersugar = struct

    fun <<(x,y) = Splayset.app y x
    fun emptyTempSet() = Splayset.empty tigertemp.compare
    fun emptyNodeSet() = Splayset.empty tigergraph.compare
    fun emptyNodeMap() = Splaymap.mkDict tigergraph.compare
    fun emptyTempMap() = Splaymap.mkDict tigertemp.compare
    fun emptyStack() = []
    fun tempSet(xs) = Splayset.addList(emptyTempSet(),xs)
    fun nodeSet(xs) = Splayset.addList(emptyNodeSet(),xs)
    val add = Splayset.add
    val addList = Splayset.addList
    fun delete(s,x) = if Splayset.member(s,x) then Splayset.delete(s,x) else s
    val union = Splayset.union
    val inter = Splayset.intersection
    val diff = Splayset.difference
    fun in_(x,s) = Splayset.member(s,x)
    fun notIn(x,s) = not (Splayset.member(s,x))
    fun isEmpty(s) = Splayset.isEmpty s
    fun isNotEmpty(s) = (not o Splayset.isEmpty) s
    fun card(s) = Splayset.numItems s
    fun equal(x,y) = Splayset.equal(x,y)
    fun get(m,x) = Splaymap.find(m,x) handle _ => raise Fail "tigersugar.get"
    val peek = Splaymap.peek
    val set = Splaymap.insert
    fun takeOne(s) = hd (Splayset.listItems s) handle _ => raise Fail "tigersugar.takeOne"
    fun push(s,x) = x::s
    fun top(s) = hd s handle _ => raise Fail "tigersugar.top"
    fun pop(s) = tl s
    fun &&(p,q) = p andalso q
    fun ||(p,q) = p orelse q

end

