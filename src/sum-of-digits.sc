import scala.annotation.tailrec

object SumOfDigits {

  @tailrec
  def digitalRoot(n: Int): Int = {
    val res = n
      .toString
      .map(_ - '0')
      .sum
    if ( n < 10 ) res else digitalRoot(res)
  }

}

SumOfDigits.digitalRoot(195)
/*
object SumOfDigits {

  def digitalRoot(n: Int): Int =
    if (n < 10) n else digitalRoot(n.toString.map(_.asDigit).sum)

}

object SumOfDigits {

  def digitalRoot(n: Int): Int = return (n - 1) % 9 + 1;

}
