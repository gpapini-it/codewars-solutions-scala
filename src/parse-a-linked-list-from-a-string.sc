object ListParser {

  case class Node(data: Int, next: Node = null)

  def parse(nodes: String): Node = nodes
    .split(" -> ") match {
    case Array("null") => null
    case Array(head, tail @ _*) => Node(head.toInt, parse(tail.mkString(" -> ")))
  }

}

ListParser.parse("12 -> null")
/*
object ListParser {
  def parse(nodes: String): Node = {
    nodes.split(" -> ").init.map(_.toInt).foldRight(null: Node)(Node)
  }
}
 */
