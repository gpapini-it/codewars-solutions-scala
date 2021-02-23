object ProdFib {

  val fibs: LazyList[Long] = 0.toLong #:: 1.toLong #:: (fibs zip fibs.tail).map { t => t._1 + t._2 }

  def productFib(prod: Long): Array[Long] =
    (fibs zip fibs.tail)
      .dropWhile(t => t._1 * t._2 < prod)
      .map(t => Array(t._1, t._2, if ( t._1 * t._2 == prod ) 1.toLong else 0.toLong))
      .head

}

ProdFib.productFib(4895L)
