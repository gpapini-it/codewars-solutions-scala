import scala.annotation.tailrec

object PrimeConsec {

  def nPrimeFactors(x: Long): Int = {
    @tailrec
    def recur(x: Long, a: Long = 2, acc: Int = 0): Int = a * a > x match {
      case false if x % a == 0 => recur(x / a, a, acc + 1)
      case false => recur(x, a + 1, acc)
      case true => acc + 1
    }

    recur(x)
  }

  def consecKprimes(k: Int, arr: Array[Long]): Int = (arr zip arr.tail)
    .count {
      case (a, b) => nPrimeFactors(a) == k && nPrimeFactors(b) == k
    }

}

PrimeConsec.consecKprimes(4, Array(10005, 10030, 10026, 10008, 10016, 10028, 10004))
