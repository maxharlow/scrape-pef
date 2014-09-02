import java.io.File
import scala.util.Try
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.immutable.ListMap
import dispatch._
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import org.json4s.DefaultFormats
import org.json4s.{JValue, JString, JBool, JNull}
import org.json4s.native.JsonMethods
import org.anormcypher.{Cypher, Neo4jREST}
import Common._

object LinkCompanies extends App {

  type JsonMap = ListMap[String, JValue]

  implicit val formats = DefaultFormats
  val opencorporatesApiToken = "jvc5BuxuEBq2NK1Vgjom"

  val http = Http configure { b =>
    b.setMaximumConnectionsTotal(50)
  }

  println("""
    ___  __     __
   / _ \/ /_ __/ /____
  / ___/ / // / __/ _ \
 /_/  /_/\_,_/\__/\___/

  """)

  Neo4jREST.setServer("localhost")

  run()

  def run() {
    for (companyNumber <- companyNumbers)
    yield for (response <- retrieve(companyNumber)) {
      val company = selectCompany(response)
      val companyOfficers = for (officers <- (response \ "officers").children)
      yield {
        val officerJson = officers \ "officer"
        val officer = selectOfficer(officerJson)
        val officership = selectOfficership(officerJson)
        (officer, officership)
      }
      load(companyNumber, company, companyOfficers)
    }
  }

  def companyNumbers: List[String] = { // todo if has no number lookup by name else lookup by number (failures fallback to lookup by name)
    val query = Cypher("MATCH (n) WHERE n.companyNumber <> '' RETURN n.companyNumber AS number").apply()
    query.map(_[String]("number")).toList
  }

  def retrieve(number: String): Try[JValue] = {
    val response = Try {
      http(url(s"https://api.opencorporates.com/companies/gb/$number?api_token=$opencorporatesApiToken") OK as.String).apply()
    }
    response recover {
      case e if e.getCause == StatusCode(404) => println(s"COMPANY NOT FOUND: $number")
      case e if e.getCause == StatusCode(403) => {
        println("HIT RATE LIMIT")
        sys.exit
      }
      case e => e.printStackTrace
    }
    response map { r =>
      JsonMethods.parse(r) \\ "company"
    }
  }

  def selectCompany(companyData: JValue): JsonMap = {
    ListMap(
      "companyName" -> companyData \ "name",
      "companyType" -> companyData \ "company_type",
      "companyRegisteredAddress" -> companyData \ "registered_address_in_full",
      "companyIncorporationDate" -> companyData \ "incorporation_date",
      "companyDissolutionDate" -> companyData \ "dissolution_date",
      "companyStatus" -> companyData \ "current_status",
      "companyInactive" -> companyData \ "inactive"
    )
  }

  def selectOfficer(officerData: JValue): JsonMap = {
    ListMap(
      "name" -> officerData \ "name"
    )
  }

  def selectOfficership(officerData: JValue): JsonMap = {
    ListMap(
      "position" -> officerData \ "position",
      "startDate" -> officerData \ "start_date",
      "endDate" -> officerData \ "end_date"
    )
  }

  def load(companyNumber: String, company: JsonMap, officers: Seq[(JsonMap, JsonMap)]): Unit = {
    val companyProps = propertise("c", company)
    val query = {
      s"MATCH (c:Organisation {companyNumber:'$companyNumber'}) SET $companyProps" +
      officers.zipWithIndex.foldLeft("\n") { case (a, ((officer, officership), i)) =>
        val officerProps = propertise("o" + i, officer)
        val officershipProps = propertise("iaoo" + i, officership)
        val officerName = tidy(officer("name").extract[String])
        val officerClass = if (officerName matches ".*[Limited|Ltd]") "Organisation" else "Individual"
        val officershipPosition = tidy(officership("position").extract[String])
        a + s"""
        |MERGE (o$i:$officerClass {name: '$officerName'}) SET $officerProps
        |MERGE (o$i)-[iaoo$i:IS_AN_OFFICER_OF {position: '$officershipPosition'}]->(c) SET $officershipProps
        """ stripMargin // todo fuzzy-match on name & correctly deal with officers that leave and then return
      }
    }
    Try(Cypher(query).apply()) recover {
      case e => println(s"FAILED TO UPDATE COMPANY $companyNumber: \n${e.getMessage}")
    }
  }

}
