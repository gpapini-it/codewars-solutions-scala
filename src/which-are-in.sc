object WhichAreIn {

  def inArray(array1: Array[String], array2: Array[String]): Array[String] =
    array1.filter(x => array2.exists(y => y.contains(x))).sorted.distinct

}
