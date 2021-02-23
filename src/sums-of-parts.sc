import scala.annotation.tailrec

object Kata {

  def partsSums(l: List[Int]): List[Int] =
    l.reverse.scanLeft(0)(_ + _).reverse

}

Kata.partsSums(List(0, 1, 3, 6, 10))
