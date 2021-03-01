import scala.annotation.tailrec

object Multiplication {

  def digits(n: Int) = {
    @tailrec
    def recur(n: Int, seq: Seq[Int]): Seq[Int] = n match {
      case 0 => seq
      case _ => recur(n / 10, seq :+ (n % 10))
    }

    if ( n == 0 ) Seq(0) else recur(n, List.empty[Int])
  }

  def persistence(n: Int) = Iterator
    .iterate(n)(digits(_).product)
    .takeWhile(_ > 9)
    .size

}

Multiplication.persistence(4)
