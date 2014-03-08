import scala.util.{Try, Success, Failure}
import java.io.File
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import scalaj.http.{Http, HttpOptions}
import org.json4s.JValue
import org.json4s.native.JsonMethods
import org.anormcypher.Cypher
import CypherTools._

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

  val companyNumbersQuery = Cypher("MATCH (n) WHERE has(n.companyNumber) RETURN n.companyNumber as number").apply()
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
          val companyObject = JsonMethods.parse(response) \\ "company"
          val company = Map(
            "companyName" -> (companyObject \ "name").string,
            "companyType" -> (companyObject \ "company_type").string,
            "companyRegisteredAddress" -> (companyObject \ "registered_address_in_full").string,
            "companyIncorporationDate" -> (companyObject \ "incorporation_date").date,
            "companyDissolutionDate" -> (companyObject \ "dissolution_date").date,
            "companyStatus" -> (companyObject \ "current_status").string,
            "companyInactive" -> (companyObject \ "inactive").boolean
          )
          val companyProperties = company.propertise("n.", "=")
          val companyDetailsResult = Cypher(s"MATCH (n {companyNumber: '$number'}) SET $companyProperties").execute()
          if (!companyDetailsResult) println(" => failed to add company details")

          // officers (directors et al)
          (companyObject \ "officers").children foreach { o =>
            val officerObject = (o \ "officer")
            val officerName = (officerObject \ "name").string.get.init.tail // unquoted!
            val officer = Map(
              "position" -> (officerObject \ "position").string,
              "startDate" -> (officerObject \ "start_date").date,
              "endDate" -> (officerObject \ "end_date").date
            )
            val officerProperties = officer.propertise()
            val officerMatchCypher = s"MATCH (o), (c {companyNumber:'$number'}) WHERE o.name =~ '(?i).*$officerName.*'"
            val officerMergeCypher = s"MERGE (o)-[:IS_AN_OFFICER_OF {$officerProperties}]->(c)"
            val officerResult = Cypher(s"$officerMatchCypher $officerMergeCypher").execute()
            if (!officerResult) println(" => failed to add officer")
          }
        }
      }
    }
  }

}
