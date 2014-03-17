import java.io.File
import scala.util.{Try, Success, Failure}
import scalaj.http.{Http, HttpOptions}
import org.json4s.DefaultFormats
import org.json4s.JValue
import org.json4s.native.JsonMethods
import org.anormcypher.Cypher
import CypherTools._

object Companies {

  implicit val formats = DefaultFormats

  def run() {
    companyNumbers foreach { number =>
      println(s"Updating data for company $number...")
      companyData(number) match {
        case Failure(e) => println(s" => failed to get data (${e.getMessage.toLowerCase})")
        case Success(companyJson) => {
          val company = getCompany(companyJson)
          addCompany(company, number)
          (companyJson \ "officers").children map { officerObject =>
            val officerJson = (officerObject \ "officer")
            val officer = getOfficer(officerJson)
            val officerName = officer.values("name").get.init.tail // unquoted
            addOfficer(officer)
            val officership = getOfficership(officerJson)
            addOfficership(officership, officerName, number)
          }
        }
      }
    }
  }

  private def companyNumbers: List[String] = {
    val companyNumbersQuery = Cypher("MATCH (n) WHERE has(n.companyNumber) RETURN n.companyNumber as number").apply()
    companyNumbersQuery.map(_[String]("number")).toList
  }

  private def companyData(number: String): Try[JValue] = {
    val apiToken = Config.openCorporatesKey
    val openCorporatesResponse = Try {
      Http(s"http://api.opencorporates.com/companies/gb/$number?api_token=$apiToken")
        .option(HttpOptions.connTimeout(2000))
        .option(HttpOptions.readTimeout(7000)).asString
    }
    openCorporatesResponse map { response =>
      JsonMethods.parse(response) \\ "company"
    }
  }

  private def getCompany(companyJson: JValue): CypherObject = {
    new CypherObject(
      "companyName" -> extractString(companyJson \ "name").string,
      "companyType" -> extractString(companyJson \ "company_type").string,
      "companyRegisteredAddress" -> extractString(companyJson \ "registered_address_in_full").string,
      "companyIncorporationDate" -> extractString(companyJson \ "incorporation_date").date("yyyy-MM-dd"),
      "companyDissolutionDate" -> extractString(companyJson \ "dissolution_date").date("yyyy-MM-dd"),
      "companyStatus" -> extractString(companyJson \ "current_status").string,
      "companyInactive" -> extractBoolean(companyJson \ "inactive").boolean
    )
  }

  private def addCompany(company: CypherObject, companyNumber: String): Unit = {
    val companyProperties = company.toUpdateString("n")
    val result = Cypher(s"MATCH (n {companyNumber:'$companyNumber'}) SET $companyProperties").execute()
    if (!result) println(" => failed to add details for company")
  }

  private def getOfficer(officerJson: JValue): CypherObject = {
    new CypherObject(
      "name" ->  titleCase(extractString(officerJson \ "name")).string
    )
  }

  private def addOfficer(officer: CypherObject): Unit = {
    val officerName = officer.values("name").get.init.tail // unquoted
    val result = if (Cypher(s"MATCH o WHERE o.name =~ '(?i).*$officerName.*' RETURN o").apply().isEmpty) {
      val officerProperties = officer.toMatchString("Individual", "o")
      Cypher(s"CREATE ($officerProperties)").execute()
    }
    else { // officer already exists
      val officerProperties = officer.toUpdateString("o")
      Cypher(s"MATCH o WHERE o.name =~ '(?i).*$officerName.*' SET $officerProperties").execute()
    }
    if (!result) println(" => failed to add officer")
  }

  private def getOfficership(officerJson: JValue): CypherObject = {
    new CypherObject(
      "position" -> extractString(officerJson \ "position").string,
      "startDate" -> extractString(officerJson \ "start_date").date("yyyy-MM-dd"),
      "endDate" -> extractString(officerJson \ "end_date").date("yyyy-MM-dd")
    )
  }

  private def addOfficership(officership: CypherObject, officerName: String, companyNumber: String): Unit = {
    val officershipProperties = officership.toMatchString("IS_AN_OFFICER_OF")
    val matchCypher = s"MATCH (o), (c:Organisation {companyNumber:'$companyNumber'}) WHERE o.name =~ '(?i).*$officerName.*'"
    val mergeCypher = s"MERGE (o)-[$officershipProperties]->(c)"
    val result = Cypher(s"$matchCypher $mergeCypher").execute()
    if (!result) println(" => failed to add officership")
  }

  private def extractString(json: JValue): String = {
    json.toOption.map(_.extract[String]).getOrElse("")
  }

  private def extractBoolean(json: JValue): String = {
    json.toOption.map(_.extract[Boolean].toString).getOrElse("")
  }

  private def titleCase(text: String): String = {
    text.split(" ").map(_.toLowerCase.capitalize).mkString(" ")
  }

}
