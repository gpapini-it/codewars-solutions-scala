
object PrefixDiff {

  sealed abstract class Expression {

    def diffBy(x: Expression): Expression

    def +(that: Expression) = Op2("+", this, that)
    def -(that: Expression) = Op2("-", this, that)
    def *(that: Expression) = Op2("*", this, that)
    def /(that: Expression) = Op2("/", this, that)
    def ^(that: Expression) = Op2("^", this, that)
    def cos = Op1("cos", this)
    def sin = Op1("sin", this)
    def tan = Op1("tan", this)
    def exp = Op1("exp", this)
    def ln = Op1("ln", this)

  }

  case class Const(el: String) extends Expression {

    override def diffBy(x: Expression) = Const("0")

    override def toString: String = el

    def toInt: Int = el.toInt
    def toLong: Long = el.toLong

    def +(that: Const) = Const((this.toLong + that.toLong).toString)
    def -(that: Const) = Const((this.toLong - that.toLong).toString)
    def *(that: Const) = Const((this.toLong * that.toLong).toString)
    def /(that: Const) = Const((this.toLong / that.toLong).toString)

  }

  case class Symbol(el: String) extends Expression {

    override def diffBy(x: Expression) = if ( this == x ) Const("1") else Const("0")

    override def toString: String = el

  }

  case class Op2(op: String, arg1: Expression, arg2: Expression) extends Expression {

    override def diffBy(x: Expression) = op match {
      case "+" => arg1.diffBy(x) + arg2.diffBy(x)
      case "-" => arg1.diffBy(x) - arg2.diffBy(x)
      case "*" => arg1.diffBy(x) * arg2 + arg1 * arg2.diffBy(x)
      case "/" => (arg1.diffBy(x) * arg2 - arg1 * arg2.diffBy(x)) / (arg2 ^ Const("2"))
    }

    override def toString: String = s"($op $arg1 $arg2)"

  }

  object Op2 {
    def apply(op: String, arg1: Const, arg2: Const): Expression = op match {
      case "+" => Const((arg1.toLong + arg2.toLong).toString)
      case "-" => Const((arg1.toLong + arg2.toLong).toString)
      case "*" => Const((arg1.toLong + arg2.toLong).toString)
      case "^" => Const((arg1.toLong + arg2.toLong).toString)
    }
  }

  case class Op1(op: String, arg: Expression) extends Expression {

    override def diffBy(x: Expression) = op match {
      case "ln" => Const("1") / arg * arg.diffBy(x)
    }

    override def toString: String = s"($op $arg)"

  }

  def diff(expr: String): String = ???

}

val x = PrefixDiff.Symbol("x")
val one = PrefixDiff.Const("1")

one - one
