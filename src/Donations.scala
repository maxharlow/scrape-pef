import java.io.File
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import org.anormcypher.Cypher
import com.github.tototoshi.csv.CSVReader
import CypherTools._

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

  def run() {
    val donations = CSVReader.open(new File(Config.donationsData)).allWithHeaders
    donations foreach { entry =>
      // benefactor
      val benefactor = Map(
        "name" -> entry("Donor name").string,
        "type" -> entry("Donor type").string,
        "postcode" -> entry("Postcode").string, // optional
        "companyNumber" -> entry("Company reg. no.").replaceAll("[^0+A-Za-z0-9]", "").replaceAll("^0*", "").string // optional
      )
      val benefactorProperties = benefactor.propertise()
      val benefactorName = benefactor("name")
      val benefactorCompanyNumber = benefactor("companyNumber")
      if (benefactorCompanyNumber.isEmpty || Cypher(s"MATCH (c {companyNumber:${benefactorCompanyNumber.get}}) RETURN c").apply().isEmpty) {
        val benefactorResult = Cypher(s"MERGE (:Benefactor {$benefactorProperties})").execute()
        if (!benefactorResult) println(" => failed to add benefactor")
      }

      // recipient
      val recipient = Map(
        "name" -> entry("Entity name").string,
        "type" -> entry("Entity type").string,
        "regulatedType" -> entry("Regulated donee type").string // optional
      )
      val recipientName = recipient("name")
      val recipientProperties = recipient.propertise()
      val recipientResult = Cypher(s"MERGE (:Recipient {$recipientProperties})").execute()
      if (!recipientResult) println(" => failed to add recipient")

      // donation
      val donation = Map(
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
      val donationProperties = donation.propertise()
      val donationMatchCypher = s"MATCH (b:Benefactor {name:${benefactorName.get}}), (r:Recipient {name:${recipientName.get}})"
      val donationCreateCypher = s"CREATE (b)-[:DONATED_TO {$donationProperties}]->(r)"
      val donationResult = Cypher(s"$donationMatchCypher $donationCreateCypher").execute()
      if (!donationResult) println(" => failed to add donation")

      println(s"Adding donation: ${benefactorName.get} -> ${recipientName.get}")
    }
  }

}
