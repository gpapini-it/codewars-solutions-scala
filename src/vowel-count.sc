object Sol {

  def getCount(inputStr : String) : Int = inputStr.count(Set('a', 'e', 'i', 'o', 'u').contains)

}

Sol.getCount("abracadabra")
