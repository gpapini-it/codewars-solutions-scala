import scala.annotation.tailrec

object EnoughIsEnough {

  def deleteNth(elements: Seq[Int], maxOccurrences: Int): Seq[Int] = {
    @tailrec
    def deleteRecurring(elements: Seq[Int], counter: Map[Int, Int], result: Seq[Int]): Seq[Int] = elements match {
      case Nil => result
      case head +: tail if counter(head) < maxOccurrences =>
        deleteRecurring(tail, counter + (head -> (counter(head) + 1)), result :+ head)
      case _ +: tail =>
        deleteRecurring(tail, counter, result)
    }

    deleteRecurring(elements, elements.map(_ -> 0).toMap, elements.empty)
  }

}
