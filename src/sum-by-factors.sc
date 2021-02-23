import scala.annotation.tailrec

object SumOfDivided {

  def factorize(x: Int): List[Int] = {
    @tailrec
    def factRecur(x: Int, a: Int = 2, list: List[Int] = Nil): List[Int] = a * a > x match {
      case false if x % a == 0 => factRecur(x / a, a, a :: list)
      case false => factRecur(x, a + 1, list)
      case true => x :: list
    }
    factRecur(x.abs)
  }

  def sumOfDivided(lst: Array[Int]) = {
    lst.map(x => x -> factorize(x).distinct)
       .flatMap {
         case (num, factors) => factors.map { _ -> num }
       }.groupBy(_._1).map { case f -> couples => f -> couples.map(_._2).sum }
       .toSeq.sorted.map { case (f, s) => s"($f $s)" }.mkString
  }

}

SumOfDivided.factorize(12)
SumOfDivided.sumOfDivided(Array(12, 15))
/*
object SumOfDivided {

  def sumOfDivided(lst: Array[Int]): String =
    (2 to lst.map(_.abs).max)
      .collect {
        case i if BigInt(i).isProbablePrime(i) && lst.count(_ % i == 0) > 0 =>
          s"($i ${lst.filter(_ % i == 0).sum})"
      }
      .mkString
}
 */
