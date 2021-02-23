import scala.annotation.tailrec

object StepInPrimes {

  def step(g: Int, m: Long, n: Long) = {
    val primes = (m to n).filter(BigInt(_).isProbablePrime(1))

    @tailrec
    def stepRecur(primes: Seq[Long]): Option[(Long, Long)] = (primes match {
      case Nil  => None
      case head +: tail => tail.collectFirst { case next if next - head >= g => next }
    }) match {
      case Some(next) if next - primes.head == g => Some((primes.head, next))
      case None if primes.isEmpty => None
      case _ => stepRecur(primes.tail)
    }

    stepRecur(primes).map { case (p1, p2) => s"($p1,$p2)" }.getOrElse("")
  }

}

StepInPrimes.step(2, 100, 110)
StepInPrimes.step(4, 30000, 100000)

StepInPrimes.step(6, 100, 110)
StepInPrimes.step(6, 100, 100)
