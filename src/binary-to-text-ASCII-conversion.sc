object Kata {

  def binaryToString(input: String): String = {
    input.grouped(8).map(Integer.parseInt(_, 2).toChar).mkString
  }

}
