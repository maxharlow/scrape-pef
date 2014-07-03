import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat

object TextTools {

  def clean(text: String): String = {
    text.filter(_ >= ' ').trim.replaceAll(" +", " ")
  }

  def stripTitles(name: String): String = {
    val prefixes = List("Ms", "Mrs", "Miss", "Mr", "Dr", "Cllr", "Sir", "Dame", "Hon", "The Hon", "Rt Hon", "The Rt Hon")
    val suffixes = List("Deceased", "QC", "MP", "MSP", "AM", "MEP", "OBE", "MBE", "CBE")
    val titlesRegex = (prefixes.map("^(" + _ + " )") ++ suffixes.map("( " + _ + ")")).mkString("|")
    name.replaceAll(titlesRegex, "").replaceAll("^(na )|( na)", "")
  }

  def stripFakePostcodes(postcode: String): String = {
    postcode.replaceAll("ZZ0 0ZZ|ZZ00ZZ|ZZ1 1ZZ|ZZ11ZZ|AA0 0AA|AA00AA|AA1 1AA|AA11AA", "")
  }

  def asDate(value: String, format: String): String = {
    val formatPattern = DateTimeFormat.forPattern(format)
    if (value.isEmpty) ""
    else DateTime.parse(value, formatPattern).toString("yyyy-MM-dd")
  }

  def asInt(value: String): String = {
    value.replaceAll("[^0-9]", "")
  }

  def asBoolean(value: String): String = {
    if (value.isEmpty) "false" else "true"
  }

}
