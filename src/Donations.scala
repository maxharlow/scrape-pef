import java.io.File
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import org.anormcypher.Cypher
import com.github.tototoshi.csv.CSVReader

object Donations {

  implicit class CypherParameterValue(v: String) {
    val value = v.trim.filter(_ >= ' ').replace("""\""", """\\""").replace("'", """\'""")
    def int = Option(value.replaceAll("[^0-9]", "")).filter(_.nonEmpty)
    def string = if (value.isEmpty) None else Some("'" + value + "'")
    def boolean = Some((!value.isEmpty).toString)
    def date = {
      val format = DateTimeFormat.forPattern("dd/MM/yyyy")
      if (value.isEmpty) None
      else Some(DateTime.parse(value, format).toString("yyyyMMdd"))
    }
  }

  def run(filename: String) {
    val donations = CSVReader.open(new File(filename)).allWithHeaders
    donations foreach { entry =>
      // benefactor
      val benefactorName = entry("Donor name").string
      val benefactorCompanyNumber = entry("Company reg. no.").replaceAll("[^0+A-Za-z0-9]", "").replaceAll("^0*", "").string // optional
      val benefactorProperties = propertise(
        "name" -> benefactorName,
        "type" -> entry("Donor type").string,
        "companyRegistrationNumber" -> benefactorCompanyNumber,
        "postcode" -> entry("Postcode").string // optional
      )
      val benefactorResult = Cypher(s"MERGE (:Benefactor {$benefactorProperties})").execute()
      if (!benefactorResult) {println(" => failed to add benefactor"); println(s"MERGE (:Benefactor {$benefactorProperties})") }

      // recipient
      val recipientName = entry("Entity name").string
      val recipientProperties = propertise(
        "name" -> recipientName,
        "type" -> entry("Entity type").string,
        "regulatedType" -> entry("Regulated donee type").string // optional
      )
      val recipientResult = Cypher(s"MERGE (:Recipient {$recipientProperties})").execute()
      if (!recipientResult) {println(" => failed to add recipient"); println(s"MERGE (:Recipient {$recipientProperties})") }

      // donation
      val donationProperties = propertise(
        "ecReference" -> entry("EC reference").string,
        "type" -> entry("Type of donation").string,
        "value" -> entry("Value").int, // in pence
        "acceptedDate" -> entry("Accepted date").date,
        "receivedDate" -> entry("Received date").date, // optional
        "reportedDate" -> entry("Reported date").date, // optional
        "nature" -> entry("Nature / Provision").string, // optional
        "purpose" -> entry("Purpose").string, // optional
        "howDealtWith" -> entry("How dealt with").string, // optional
        "recordedBy" -> entry("Rec'd by (AU)").string, // optional
        "reportedUnder6212" -> entry("Reported under 62:12").string, // optional
        "isSponsorship" -> entry("Is sponsorship").boolean,
        "complianceBreach" -> entry("Compliance breach").string
      )
      val matchCypher = s"MATCH (b:Benefactor {name:${benefactorName.get}}), (r:Recipient {name:${recipientName.get}})"
      val mergeCypher = s"MERGE (b)-[:DONATED_TO {$donationProperties}]->(r)"
      val donationResult = Cypher(s"$matchCypher $mergeCypher").execute()
      if (!donationResult) {println(" => failed to add donation"); println(matchCypher+" "+mergeCypher)}

      println(s"Adding donation: ${benefactorName.get} -> ${recipientName.get}")
    }
  }

  def propertise(values: (String, Option[String])*): String = {
    val properties = values collect {
      case (key, Some(value)) => s"$key:$value"
    }
    properties mkString ","
  }

}
