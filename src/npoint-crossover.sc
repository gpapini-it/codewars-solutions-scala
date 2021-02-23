
object NPointCrossover {

  def crossover[T](ns: List[Int], xs: List[T], ys: List[T]): (List[T], List[T]) = ns.sorted.distinct match {
    case Nil => (xs, ys)
    case head +: tail => crossover(tail, xs.take(head) ++ ys.drop(head), ys.take(head) ++ xs.drop(head))
  }

}
