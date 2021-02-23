
object Kata {

  def multiplicationTable(size: Int): List[List[Int]] = (1 to size).map(a => (1 to size).map(b => a * b).toList).toList

}
