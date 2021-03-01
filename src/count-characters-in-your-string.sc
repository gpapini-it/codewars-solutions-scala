
object Kata {
  def count(string: String): Map[Char, Int] = string.groupBy(identity).map { case c -> g => c -> g.length }
}
