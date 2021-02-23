object HumanTime {

  def formatDuration(seconds: Int): String = {

    val s = seconds % 60
    val m = (seconds / 60) % 60
    val h = (seconds / 3600) % 24
    val d = (seconds / (3600 * 24)) % 365
    val y = seconds / (3600 * 24 * 365)

    (Seq(y, d, h, m, s) zip Seq("year", "day", "hour", "minute", "second"))
      .filter(_._1 > 0)
      .map {
        case (n, t) => if ( n > 1 ) (n, t + "s") else (n, t)
      }
      .map(x => x._1 + " " + x._2)
    match {
      case Nil => "now"
      case Seq(x) => x
      case elem :+ last => elem.mkString(", ") + " and " + last
    }

  }

}

HumanTime.formatDuration(15731080)
