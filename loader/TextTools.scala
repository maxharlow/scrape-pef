object TextTools {

  def clean(text: String): String = {
    text.filter(_ >= ' ').trim.replaceAll(" +", " ")
  }

  def stripTitles(name: String): String = {
    val prefixes = List("Ms", "Mrs", "Miss", "Mr", "Dr", "Cllr", "Sir", "Dame", "Hon", "The Hon", "Rt Hon", "The Rt Hon")
    val suffixes = List("QC", "MP", "MSP", "AM", "MEP", "OBE", "MBE", "CBE")
    val titlesRegex = (prefixes.map("^(" + _ + " )") ++ suffixes.map("( " + _ + ")")).mkString("|")
    name.replaceAll(titlesRegex, "").replaceAll("^(na )|( na)", "")
  }

}
