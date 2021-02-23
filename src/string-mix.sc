import scala.math.Ordered.orderingToOrdered

object StringMix {

  case class CharFromString(char: Char, freq: Int, source: String) extends Ordered[CharFromString] {

    val thisAsString: String = s"$source:" + s"$char" * freq

    def against(c2: CharFromString): CharFromString = c2 match {
      case CharFromString(_, f, _) if f == freq => CharFromString(char, freq, "=")
      case CharFromString(_, f, _) if f < freq => CharFromString(char, freq, source)
      case CharFromString(_, f, s) => CharFromString(char, f, s)
    }

    override def toString: String = thisAsString
    override def compare(that: CharFromString): Int = (-this.freq, this.toString).compare(-that.freq, that.toString)

  }

  def string2Map(s: String) = s
    .replaceAll("""[^a-z]""", "")
    .groupBy(identity)
    .map { case k -> v => k -> v.length }

  /* Generalized to n strings  */
  def mix(ss: String*) /*: String*/ = ss
    .map(string2Map)
    .map(_.filter { case (_, v) => v > 1 })
    .zipWithIndex
    .flatMap {
      case (m, ind) => m.map { case (char, freq) => CharFromString(char, freq, (ind + 1).toString) }
    }.groupBy { _.char }.map { case (_, group) => group.reduce(_ against _) }.toSeq.sorted.mkString("/")

}

StringMix.mix("Lords of the Fallen", "gamekult")
