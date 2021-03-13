object Kata {

  def groupByCommas(n: Int): String = n
    .toString
    .reverse
    .grouped(3)
    .map(_.mkString)
    .mkString(",")
    .reverse

}

Kata.groupByCommas(1212)
