
object DblLinear {

  def dblLinear(n: Int): Int = {
    var ai = 0
    var bi = 0
    var eq = 0
    var sequence = Array(1)
    while ( ai + bi < n + eq ) {
      val y = 2 * sequence(ai) + 1
      val z = 3 * sequence(bi) + 1
      if ( y < z ) {
        sequence :+= y
        ai += 1
      } else if ( y > z ) {
        sequence :+= z
        bi += 1
      } else {
        sequence :+= y
        ai += 1
        bi += 1
        eq += 1
      }
    }
    sequence.last
  }

}

(0 to 10).map(DblLinear.dblLinear)

/*
object DblLinear {

import scala.collection.SortedSet

  def dblLinear(n: Int, result: SortedSet[Int] = SortedSet(1)): Int = {
    val head = result.head
    if (n == 0) head
    else dblLinear(n - 1, result.tail ++ SortedSet(head * 2 + 1, head * 3 + 1 ))
  }
}
