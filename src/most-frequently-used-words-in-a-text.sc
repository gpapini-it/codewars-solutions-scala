
object Kata {

  def mostFrequentlyUsedWords(text: String) = text
    .toLowerCase()
    .replaceAll("""[\p{Punct}&&[^']]""", " ")
    .split("""\s+""")
    .filterNot(_ isEmpty)
    .groupBy(identity)
    .transform { case (_, g) => g.length }
    .toSeq
    .sortBy { case (_, v) => -v }
    .take(3)
    .map(_._1)

}

Kata.mostFrequentlyUsedWords(
  """In a village of La Mancha, the name of which I have no desire to call to
    |mind, there lived not long since one of those gentlemen that keep a lance
    |in the lance-rack, an old buckler, a lean hack, and a greyhound for
    |coursing. An olla of rather more beef than mutton, a salad on most
    |nights, scraps on Saturdays, lentils on Fridays, and a pigeon or so extra
    |on Sundays, made away with three-quarters of his income.""".stripMargin)


Kata.mostFrequentlyUsedWords(
  "e e e e DDD ddd DdD: ddd ddd aa aA Aa, bb cc cC e e e")

Kata.mostFrequentlyUsedWords(
  "  //wont won't won't")
