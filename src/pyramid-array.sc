object Kata {

  def pyramid(n: Int) =
    (1 to n).map(List.fill(_)(1)).toList

}

Kata.pyramid(3)
