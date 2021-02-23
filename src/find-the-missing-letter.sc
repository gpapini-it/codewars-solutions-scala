object Kata {

  def findMissingLetter(chars: Array[Char]) = {
    (chars zip chars.tail)
      .dropWhile(t => t._2 - t._1 == 1)
      .head
      ._1 + 1
  }.toChar

}

Kata.findMissingLetter(Array('O', 'Q', 'R', 'S'))

object Kata {

  def findMissingLetter(chars: Array[Char]) =
    chars.head to chars.last diff chars /*head*/

}

Kata.findMissingLetter(Array('O', 'Q', 'R', 'S'))
