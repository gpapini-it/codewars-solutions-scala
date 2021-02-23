
object Tribonacci {

  def tribonacci[T : Numeric](signature : List[T], n : Int) = {
    signature.take(math.min(n, signature.length)) ++
    Iterator.iterate(signature)(s => s.tail :+ s.sum)
            .take(n - signature.length)
            .map(_.sum)
            .toList
  }

}

Tribonacci.tribonacci(List(1, 1, 1), 10)

/*
 object Tribonacci {
  def tribonacci[T : Numeric](sig: List[T], n: Int): List[T] =
    if (n <= 3) sig take n else sig.head +: tribonacci (sig.tail :+ sig.sum, n-1)
}
*/
