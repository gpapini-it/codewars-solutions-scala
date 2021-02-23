
object WeightSort {

  def orderWeight(s: String) = {
    val names = s.trim.split("\\s+")
    val weights = names.map(_.map(_ - '0').sum)

    (weights zip names).sorted.unzip._2.mkString(" ")
  }

}

WeightSort.orderWeight("103 123 4444 99 2000")
