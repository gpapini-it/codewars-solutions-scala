
object FindTheOddInt {

  def findOdd(xs: Seq[Int]) = xs
    .groupBy(identity)
    .transform((k, v) => v.length)
    .filter(_._2 % 2 == 1)
    .keys
    .head

}

FindTheOddInt.findOdd(List(20, 1, -1, 2, -2, 3, 3, 5, 5, 1, 2, 4, 20, 4, -1, -2, 5))

object FindTheOddInt {

  def findOdd(xs: Seq[Int]): Int = xs.reduce(_ ^ _)

}
