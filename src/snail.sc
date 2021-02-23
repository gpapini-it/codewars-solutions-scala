import scala.annotation.tailrec

object Snail {

  object Direction extends Enumeration {
    type Direction = Value
    val South = Value("South")
    val East = Value("East")
  }

  import Direction._

  def snail(xs: List[List[Int]]): List[Int] = {
    @tailrec
    def snailRecur(xs: List[List[Int]], dir: Direction, seq: List[Int]): List[Int] = {
      if ( xs.isEmpty || xs.head.isEmpty ) seq else dir match {
        case East => xs match {
          case (row1head +: Nil) +: otherRows => snailRecur(otherRows, South, seq :+ row1head)
          case (row1head +: row1tail) +: otherRows => snailRecur(row1tail +: otherRows, East, seq :+ row1head)
        }
        case South => snailRecur(xs.transpose.reverse, East, seq)
      }
    }

    snailRecur(xs, East, List.empty)
  }

}

Snail.snail(List(
  List(1, 2, 3, 4),
  List(4, 5, 6, 7),
  List(7, 8, 9, 10)))

/*
object Snail {

  def snail(xs: List[List[Int]]): List[Int] = xs match {
    case Nil => Nil
    case x :: xs => x ++ snail(xs.transpose.reverse)
  }

}
