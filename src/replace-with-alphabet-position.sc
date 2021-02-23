
object Kata {

  def alphabetPosition(text: String) = text
    .filter(_.isLetter)
    .map(_.toLower - 'a' + 1)
    .mkString(" ")

}

Kata.alphabetPosition("abc")
