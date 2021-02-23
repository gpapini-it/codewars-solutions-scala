import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object Kata {

  def josephusSurvivor(n: Int, k: Int) = {
    @tailrec
    def turn(seq: ListBuffer[Int], pos: Int, k: Int): Int = {
      if ( seq.size == 1 ) {
        seq.head
      } else {
        val newPos = (pos + k) % seq.size
        seq.remove(newPos)
        turn(seq, newPos, k)
      }
    }

    turn(ListBuffer.range(1, n + 1), 0, k - 1)
  }

}

Kata.josephusSurvivor(7, 3)
