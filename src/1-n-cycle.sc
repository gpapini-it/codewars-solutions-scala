object Cycle {

  def cycle(n: Int) = n match {
    case _ if n % 2 == 0 || n % 5 == 0 => -1
    case _ => 1 + Iterator.iterate(10 % n)(_ * 10 % n).takeWhile(x => x != 1).size
  }

}

Cycle.cycle(21)
