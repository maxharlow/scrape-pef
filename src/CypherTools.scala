import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat

object CypherTools {

  implicit class CypherParameterValue(value: String) {
    def int = Option(value.replaceAll("[^0-9]", "")).filter(_.nonEmpty)
    def string = if (value.isEmpty) None else Some("'" + value + "'")
    def boolean = Some((!value.isEmpty).toString)
    def date(format: String) = {
      val formatPattern = DateTimeFormat.forPattern(format)
      if (value.isEmpty) None
      else Some(DateTime.parse(value, formatPattern).toString("yyyyMMdd"))
    }
  }

  class CypherObject(properties: (String, Option[String])*) {

    val values = properties.toMap

    def toMatchString(kind: String = "", id: String = "") = {
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

  // deprecated
  implicit class CypherPropertiesMap(v: Map[String, Option[String]]) {
    def propertise(prefix: String = "", separator: String = ":", delimiter: String = ","): String = {
      val properties = v collect {
        case (key, Some(value)) => s"$prefix$key$separator$value"
      }
      properties mkString delimiter
    }
  }

}
