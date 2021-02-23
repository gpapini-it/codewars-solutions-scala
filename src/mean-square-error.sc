object Sol {

  def solution(a: Array[Int], b: Array[Int]): Double =
    {a zip b}
      .map(x => x._1 - x._2)
      .map(x => x * x)
      .sum.toDouble / a.length

}

Sol.solution(Array(10, 20, 10, 2), Array(10, 25, 5, -2))
