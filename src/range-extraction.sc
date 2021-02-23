import scala.annotation.tailrec

object Kata {

  @tailrec
  def spanRange(list: Seq[Int], range: Seq[Int] = Nil): (Seq[Int], Seq[Int]) = {
    (range, list) match {
      case (Nil, Nil) => (list, range)
      case (Nil, a +: tail) => spanRange(tail, range :+ a)
      case (_ :+ a, b +: tail) if b - a == 1 => spanRange(tail, range :+ b)
      case (_, outOfRange) => (range, outOfRange)
    }
  }

  @tailrec
  def aggregateRanges(list: Seq[Int], ranges: Seq[Seq[Int]] = Nil): Seq[Seq[Int]] = list match {
    case Nil => ranges
    case notEmpty => spanRange(list) match {
      case (range, Nil) => ranges :+ range
      case (range, rest) => aggregateRanges(rest, ranges :+ range)
    }
  }

  def solution(xs: Seq[Int]) = aggregateRanges(xs)
    .map {
      case Seq(a) => a.toString
      case seq @ Seq(_, _) => seq.mkString(",")
      case first +: _ :+ last => first.toString + "-" + last.toString
    }.mkString(",")

}

val lst = List(-58, -59, 31, 32, 92, -51, 86, 87, 88, 51, 52, 53, 54, 55, 56, 57, -53, -62, -65, -64, -63, -62, -61, 77, 56, -18, -17, -65, -64, -63, 4, 21, 22, 23, 24, 25, 26, 27, -46, -45, -44, -43, -42, 52, 45, -64, -33, 0, 0, -22, -25, 69, -86, 20, 21, 22, 46, 47, 48, 49, 50, 51, 52)
Kata.solution(lst)

/*
object Kata {
  def solution(nums: List[Int]): String = {
    nums.foldLeft(List.empty[List[Int]]) {
      case (Nil, x) => List(x)::Nil
      case (xs::xxs, x) =>
        if (xs.head == x-1) (x::xs)::xxs
        else List(x)::xs::xxs
    }.map {
      case x::Nil => s"$x"
      case x::y::Nil => s"$y,$x"
      case x => s"${x.last}-${x.head}"
    }.reverse.mkString(",")
  }
}
 */
