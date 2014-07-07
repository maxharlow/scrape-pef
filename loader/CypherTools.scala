import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat

object CypherTools {

  implicit class CypherParameterValue(v: String) {
    val value = v.replace("""\""", """\\\\""").replace("'", """\'""")
    def int = Option(value).filter(_.nonEmpty)
    def string = if (value.isEmpty) None else Some("'" + value + "'")
    def boolean = Some(value.toBoolean.toString)
    def date(format: String) = {
      val formatPattern = DateTimeFormat.forPattern(format)
      if (value.isEmpty) None
      else Some(DateTime.parse(value, formatPattern).toString("yyyyMMdd"))
    }
  }

  class CypherObject(kind: String)(properties: (String, Option[String])*) {

    val values = properties.toMap

    def toMatchString(id: String = "") = {
      val propertiesList = propertise("", ":", ",")
      s"$id:$kind {$propertiesList}"
    }

    def toUpdateString(id: String = "") = {
      propertise(id + ".", "=", ",")
    }

    private def propertise(prefix: String, separator: String, delimiter: String): String = {
      val validProperties = properties collect {
        case (key, Some(value)) => s"$prefix$key$separator$value"
      }
      validProperties mkString delimiter
    }

  }

}
