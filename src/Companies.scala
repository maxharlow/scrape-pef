import scala.util.{Try, Success, Failure}
import java.io.File
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import scalaj.http.{Http, HttpOptions}
import org.json4s.JValue
import org.json4s.native.JsonMethods
import org.anormcypher.Cypher

object Companies {

  implicit val formats = org.json4s.DefaultFormats

  implicit class CypherParameterValue(value: JValue) {
    def int = Option(value.extract[Int]).map(_.toString)
    def string = Option(value.extract[String]).map("'" + _.trim.replace("""\""", """\\""").replace("'", """\'""") + "'")
    def boolean = Option(value.extract[Boolean]).map(_.toString)
    def date = Option(value.extract[String]) map { s =>
      val format = DateTimeFormat.forPattern("yyyy-MM-dd")
      DateTime.parse(s, format).toString("yyyyMMdd")
    }
  }

  val companyNumbersQuery = Cypher("MATCH (n) WHERE has(n.companyRegistrationNumber) RETURN n.companyRegistrationNumber as number").apply()
  val companyNumbers = companyNumbersQuery.map(_[String]("number")).toList

  def run() {
    companyNumbers foreach { number =>
      val apiToken = Config.openCorporatesKey
      Try {
        Http(s"http://api.opencorporates.com/companies/gb/$number?api_token=$apiToken")
          .option(HttpOptions.connTimeout(2000))
          .option(HttpOptions.readTimeout(7000)).asString
      }
        match {
        case Failure(e) => println(s"Failed to get data for company $number (${e.getMessage.toLowerCase})")
        case Success(response) => {
          println(s"Updating data for company $number...")

          // company details
          val company = JsonMethods.parse(response) \\ "company"
          val companyProperties = propertise("n.", "=")(
            "companyName" -> (company \ "name").string,
            "companyType" -> (company \ "company_type").string,
            "companyRegisteredAddress" -> (company \ "registered_address_in_full").string,
            "companyIncorporationDate" -> (company \ "incorporation_date").date,
            "companyDissolutionDate" -> (company \ "dissolution_date").date,
            "companyStatus" -> (company \ "current_status").string,
            "companyInactive" -> (company \ "inactive").boolean
          )
          val companyDetailsResult = Cypher(s"MATCH (n {companyRegistrationNumber: '$number'}) SET $companyProperties").execute()
          if (!companyDetailsResult) println(" => failed to add company details")

          // officers (directors et al)
          (company \ "officers").children foreach { o =>
            val officer = (o \ "officer")
            val officerName = (officer \ "name").string.get.init.tail // unquoted!
            val officerProperties = propertise()(
              "position" -> (officer \ "position").string,
              "startDate" -> (officer \ "start_date").date,
              "endDate" -> (officer \ "end_date").date
            )
            val matchQuery = s"MATCH (o), (c {companyRegistrationNumber:'$number'}) WHERE o.name =~ '(?i).*$officerName.*'"
            val mergeQuery = s"MERGE (o)-[:IS_AN_OFFICER_OF {$officerProperties}]->(c)"
            val officerResult = Cypher(s"$matchQuery $mergeQuery").execute()
            if (!officerResult) println(" => failed to add officer relationship")
          }
        }
      }
    }
  }

  def propertise(prefix: String = "", separator: String = ":", delimiter: String = ",")(values: (String, Option[String])*): String = {
    val properties = values collect {
      case (key, Some(value)) => s"$prefix$key$separator$value"
    }
    properties mkString delimiter
  }

}
