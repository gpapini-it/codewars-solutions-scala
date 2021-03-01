
object Opstrings {

  def diag1Sym(strng: String) =
    strng.split("\n").map(_.toCharArray)
         .transpose.map(_.mkString).mkString("\n")
  def rot90Clock(strng: String) =
    strng.split("\n").map(_.toCharArray)
         .reverse.transpose.map(_.mkString).mkString("\n")
  def selfieAndDiag1(strng: String) =
    strng.split("\n")
         .zip(diag1Sym(strng).split("\n"))
         .map { case (a, b) => a + "|" + b }
         .mkString("\n")

  def oper(f: String => String, s: String): String = f(s)

}

Opstrings.selfieAndDiag1("abcd\nefgh\nijkl\nmnop")
