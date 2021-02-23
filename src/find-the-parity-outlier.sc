import scala.annotation.tailrec

object Parity {

  @tailrec
  def findOutlier(integers: List[Int]): Int =
    integers.map(x => math.abs(x % 2)).take(3) match {
      case List(0, 1, 1) | List(1, 0, 0) => integers(0)
      case List(1, 0, 1) | List(0, 1, 0) => integers(1)
      case List(1, 1, 0) | List(0, 0, 1) => integers(2)
      case _ => findOutlier(integers.tail)
    }

}

Parity.findOutlier(List(2, 4, 6, 8, 10, 3))
Parity.findOutlier(List(2, -6, 8, -10, -3))

/*
object Parity {

  def findOutlier(integers : List[Int]) : Int =
    integers.partition(_ % 2 == 0) match {
      case (List(outlier), _) => outlier
      case (_, List(outlier)) => outlier
    }

}

object Parity {

  def findOutlier(integers : List[Int]) : Int = {
    val (even, odd) = integers.partition(_ % 2 == 0)
    if ( even.size == 1 ) even.head else odd.head
  }

}

object Parity {

  def findOutlier(integers : List[Int]) : Int = {

    def evenList(lst : List[Int]) = (lst take 3 count isEven) >= 2

    def isEven(x : Int) = x % 2 == 0

    if ( evenList(integers) ) {
      integers filterNot isEven head
    } else {
      integers filter isEven head
    }

  }

}
