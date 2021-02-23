object Sol {

  var fibMemoized = Map(0 -> BigInt(0), 1 -> BigInt(1))

  def fib(n: Int): BigInt = {
    val res = fibMemoized getOrElse(n, fib(n - 2) + fib(n - 1))
    fibMemoized += n -> res
    return res
  }

}
