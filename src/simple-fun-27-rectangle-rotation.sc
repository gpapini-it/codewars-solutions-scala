
object Kata {

  def rectangleRotation(a: Int, b: Int): Int = {
    val ac: Int = (a / 2.0 / math.sqrt(0.5)).floor.toInt
    val bc: Int = (b / 2.0 / math.sqrt(0.5)).floor.toInt
    (ac * 2 + 1) * (bc * 2 + 1) / 2 + (1 - (ac + bc) % 2)
  }

}
