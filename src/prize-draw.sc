object PrizeDraw {

  def nthRank(st: String, we: Array[Int], n: Int) =
    if (st.isEmpty) "No participants"
    else if (n > we.length) "Not enough participants"
    else st
    .split(",")
    .map(x => x -> (x.length + x.toLowerCase.map(_ - 'a' + 1).sum))
    .zip(we)
    .map { case (fn -> som, w) => fn -> som * w }
    .sortBy { case fn -> wn => (-wn, fn) }
    .apply(n - 1)
    ._1

}

PrizeDraw.nthRank("Addison,Jayden,Sofia,Michael,Andrew,Lily,Benjamin", Array(4, 2, 1, 4, 3, 1, 2), 8)
