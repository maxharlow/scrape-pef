
object CypherTools {

  implicit class CypherPropertiesMap(v: Map[String, Option[String]]) {
    def propertise(prefix: String = "", separator: String = ":", delimiter: String = ","): String = {
      val properties = v collect {
        case (key, Some(value)) => s"$prefix$key$separator$value"
      }
      properties mkString delimiter
    }
  }

  // implicit class CypherParameterValue(value: JValue) {
  //   def int = Option(value.extract[Int]).map(_.toString)
  //   def string = Option(value.extract[String]).map("'" + _.trim.replace("""\""", """\\""").replace("'", """\'""") + "'")
  //   def boolean = Option(value.extract[Boolean]).map(_.toString)
  //   def date = Option(value.extract[String]) map { s =>
  //     val format = DateTimeFormat.forPattern("yyyy-MM-dd")
  //     DateTime.parse(s, format).toString("yyyyMMdd")
  //   }
  // }

}
