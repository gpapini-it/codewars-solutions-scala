import scala.annotation.tailrec

object Arge {

  @tailrec private def nbYearRecursive(p0: Int, percent: Double, aug: Int, p: Int, year: Int = 0): Int =
    if ( p0 >= p ) {
      year
    } else {
      nbYearRecursive((p0 * (1 + percent / 100)).toInt + aug, percent, aug, p, year + 1)
    }

  def nbYear(p0: Int, percent: Double, aug: Int, p: Int): Int =
    nbYearRecursive(p0, percent, aug, p, 0)

}

Arge.nbYear(1500, 5, 100, 5000)
