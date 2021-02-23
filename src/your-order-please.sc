object Text {

  def order(str: String): String = str
    .split(" ").map {
    case word => ("""\d+""".r).findFirstIn(word).getOrElse("0").toInt -> word
  }.sorted.unzip._2.mkString(" ")

}
