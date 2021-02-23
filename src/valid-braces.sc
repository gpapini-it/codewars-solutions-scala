import scala.annotation.tailrec

object Kata {

  @tailrec
  def validBraces(s : String) : Boolean = {
    val res = s
      .replace("()", "")
      .replace("[]", "")
      .replace("{}", "")
    if ( res.isEmpty ) {
      true
    } else if ( res.length == s.length ) {
      false
    } else {
      validBraces(res)
    }
  }

}

Kata.validBraces("()()()")
Kata.validBraces("(){}[]")
Kata.validBraces("([{}])")

object Kata {

  @tailrec
  def validBraces(s : String) : Boolean = s
    .replace("()", "")
    .replace("[]", "")
    .replace("{}", "")
  match {
    case "" => true;
    case `s` => false;
    case x => validBraces(x)
  }

}
