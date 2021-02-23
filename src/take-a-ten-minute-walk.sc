object Solution {

  val northSouthMap = Map('n' -> 1, 's' -> -1)
  val eastWestMap = Map('e' -> 1, 'w' -> -1)

  def isValidWalk(walk : Seq[Char]) : Boolean = walk.length == 10 &&
    walk.map(northSouthMap getOrElse(_, 0)).sum == 0 &&
    walk.map(eastWestMap getOrElse(_, 0)).sum == 0

}

Solution.isValidWalk(Seq('n', 'n', 'n', 's', 'n', 's', 'n', 's', 'n', 's'))
