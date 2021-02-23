import scala.annotation.tailrec

object Solution {

  def isprime(n: Long) = n match {
    case lt1 if lt1 <= 1 => false
    case 2L => true
    case gt2 => (2L to math.sqrt(gt2).toLong).forall(p => gt2 % p != 0)
  }

  @tailrec
  def digits(n: Long, prevdigits: Seq[Int] = Seq.empty[Int]): Seq[Int] = n match {
    case 0 if (prevdigits.isEmpty) => prevdigits :+ 0
    case 0 => prevdigits
    case n => digits(n / 10, prevdigits :+ (n % 10).toInt)
  }

  def reverse(n: Long) = digits(n).foldLeft(0L) { case (l, r) => l * 10 + r }

  def isbackwardprime(n: Long) = {
    val revn = reverse(n)
    revn != n && isprime(n) && isprime(revn)
  }

  def backwardsPrime(start: Long, end: Long): String = {
    (start to end).filter(isbackwardprime).mkString(",")
  }

}

Solution.backwardsPrime(100, 403)
