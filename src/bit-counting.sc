object Kata {

  def countBits(n: Int) = Iterator
    .iterate(n)(_ / 2)
    .takeWhile(_ > 0)
    .count(_ % 2 == 1)

}

Kata.countBits(1234)

/*
object Kata {
  def countBits(n: Int): Int = Integer.bitCount(n)
}
*/
