
object RevRot {

  def cubeDigitSumIsEven(str: String): Boolean =
    str.map(_ - '0').map(math.pow(_, 3).toInt).sum % 2 == 0

  def revOrRotSingle(str: String): String =
    if ( cubeDigitSumIsEven(str) ) str.reverse else str.tail + str.head

  def revRot(str: String, sz: Int): String =
    if ( sz <= 0 ) "" else str.grouped(sz).filter(_.length == sz).map(revOrRotSingle).mkString("")

}

RevRot.revRot("733049910872815764", 5)

/*
object RevRot {

  def revRot(strng : String, sz : Int) : String = {
    if ( sz <= 0 || strng.isEmpty || sz > strng.length ) return ""
    strng.dropRight(strng.length % sz).grouped(sz).map { chunk =>
      if ( chunk.map(_.toInt).sum % 2 == 0 ) chunk.reverse else chunk.tail + chunk.head
    }.mkString
  }

}
