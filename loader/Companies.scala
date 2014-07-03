import java.io.File
import scala.util.{Try, Success, Failure}
import org.json4s.DefaultFormats
import org.json4s.JValue
import org.json4s.native.JsonMethods
import org.anormcypher.{Cypher, Neo4jREST}
import CypherTools._
import Utils._

object Companies {

  Neo4jREST.setServer("localhost")

  implicit val formats = DefaultFormats
  val opencorporatesApiToken = ""

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
            val officership = getOfficership(officerJson)
            addOfficer(officer)
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
    request(s"https://api.opencorporates.com/companies/gb/$number?api_token=$opencorporatesApiToken") map { response =>
      JsonMethods.parse(response) \\ "company"
    }
  }

  private def getCompany(companyJson: JValue): CypherObject = {
    new CypherObject("Organisation")(
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
    new CypherObject("Individual")(
      "name" ->  titleCase(extractString(officerJson \ "name")).string
    )
  }

  private def addOfficer(officer: CypherObject): Unit = {
    val officerName = officer.values("name").get.init.tail // unquoted
    if (Cypher(s"MATCH o WHERE o.name =~ '(?i).*$officerName.*' RETURN o").apply().isEmpty) {
      val officerProperties = officer.toMatchString("o")
      val result = Cypher(s"CREATE ($officerProperties)").execute()
      if (!result) println(" => failed to add officer")
    } // (no update otherwise)
  }

  private def getOfficership(officerJson: JValue): CypherObject = {
    new CypherObject("OFFICER_OF")(
      "position" -> extractString(officerJson \ "position").string,
      "startDate" -> extractString(officerJson \ "start_date").date("yyyy-MM-dd"),
      "endDate" -> extractString(officerJson \ "end_date").date("yyyy-MM-dd")
    )
  }

  private def addOfficership(officership: CypherObject, officerName: String, companyNumber: String): Unit = {
    val officershipProperties = officership.toMatchString()
    val matchCypher = s"MATCH (o), (c:Organisation {companyNumber:'$companyNumber'}) WHERE o.name =~ '(?i).*$officerName.*'"
    val mergeCypher = s"MERGE (o)-[$officershipProperties]->(c)"
    val result = Cypher(s"$matchCypher $mergeCypher").execute()
    if (!result) println(" => failed to add officership")
  }

  private def extractString(json: JValue): String = {
    json.noNulls.toOption.map(_.extract[String]).getOrElse("")
  }

  private def extractBoolean(json: JValue): String = {
    json.noNulls.toOption.map(_.extract[Boolean].toString).getOrElse("")
  }

  private def titleCase(text: String): String = {
    text.split(" ").map(_.toLowerCase.capitalize).mkString(" ")
  }

}
