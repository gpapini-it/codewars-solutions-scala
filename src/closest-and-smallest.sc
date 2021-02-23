object ClosestWeight {

  def weight(n: String) = n.map { _.asDigit }.sum

  def closest(str: String) /*: Array[(Int, Int, Int)] */ = {
    val numStrs = str split " " filterNot (_.isEmpty)
    val numbers = numStrs map { _.toInt }
    val weights = numStrs map { weight }
    val triples = (numbers.zip(weights).zipWithIndex) map {
      case ((n, w), i) => (w, i, n)
    }
    val combos = for ( (a, i) <- triples.zipWithIndex; (b, j) <- triples.zipWithIndex; if i != j )
      yield Array(a, b)

    if ( !combos.isEmpty ) (combos minBy {
      case Array((aw, ai, an), (bw, bi, bn)) => ((aw - bw).abs, aw + bw, ai + bi)
    }).sorted
    else Array[(Int, Int, Int)]()
  }

}

ClosestWeight.closest("")
ClosestWeight.closest("102")
ClosestWeight.closest("456899 50 11992 176 272293 163 389128 96 290193 85 52")

/*
object ClosestWeight {

  def closest(input: String): Array[(Int, Int, Int)] = {
    input.split(" ") match {
      case subs if subs.size < 2 =>
        Array()
      case subs =>
        subs
          .zipWithIndex
          .map { case (s, i) => (s.map(_.asDigit).sum, i, s.toInt) }
          .combinations(2)
          .map(comb => comb.sortBy(x => (x._1, x._2)))
          .minBy { case Array((w1, i1, _), (w2, i2, _)) => ((w1 - w2).abs, w1 + w2, i1 + i2) }
    }
  }
}
 */
