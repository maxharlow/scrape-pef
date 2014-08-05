import java.io.File
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.collection.immutable.ListMap
import dispatch._
import dispatch.Defaults._
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

  def companyNumbers: List[String] = {
    val query = Cypher("MATCH (n) WHERE has(n.companyNumber) RETURN n.companyNumber AS number").apply()
    query.map(_[String]("number")).toList
  }

  def retrieve(number: String): Future[JValue] = {
    val response = Http {
      url(s"https://api.opencorporates.com/companies/gb/$number?api_token=$opencorporatesApiToken") OK as.String
    }
    Await.ready(response, 1.minute)
    response map { r => //todo: print message on failures
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

  def propertise(bind: String, jsonMap: JsonMap): String = {
    val pairs = jsonMap map {
      case (key, JString(value)) if key contains "Date" => s"""\n  $bind.$key=${value.replace("-", "")}"""
      case (key, JString(value)) => s"\n  $bind.$key='${tidy(value)}'"
      case (key, JBool(value)) => s"\n  $bind.$key=${value.toString}"
      case (key, JNull) => ""
      case _ => throw new Exception("Unexpected Json!")
    }
    pairs.filter(!_.isEmpty).mkString(",")
  }

  def load(companyNumber: String, company: JsonMap, officers: Seq[(JsonMap, JsonMap)]): Unit = {
    val companyProps = propertise("c", company)
    val query = {
      s"MATCH (c:Organisation {companyNumber:'$companyNumber'}) SET $companyProps" +
      officers.zipWithIndex.foldLeft("\n") { case (a, ((officer, officership), i)) =>
        val officerProps = propertise("o" + i, officer)
        val officershipProps = propertise("iaoo" + i, officership)
        val officerName = tidy(officer("name").extract[String])
        a + s"""
        |MERGE (o$i:Individual {name: '$officerName'}) SET $officerProps
        |MERGE (o$i)-[iaoo$i:IS_AN_OFFICER_OF]->(c) SET $officershipProps
        """ stripMargin // todo fuzzy-match on name
      }
    }
    val result = Cypher(query).execute()
    if (!result) {
      println(s"FAILED TO UPDATE COMPANY $companyNumber:\n")
      println(query)
    }
  }

}
