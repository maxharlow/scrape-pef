import java.io.File
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import org.anormcypher.Cypher
import com.github.tototoshi.csv.CSVReader

object Grapher extends App {

  implicit class CypherParameterValue(v: String) {
    val value = v.trim.filter(_ >= ' ')
    def int = Option(value.replaceAll("[^0-9]", "")).filter(_.nonEmpty)
    def string = if (value.isEmpty) None else Some("'" + value + "'")
    def boolean = Some((!value.isEmpty).toString)
    def date = {
      val format = DateTimeFormat.forPattern("dd/MM/yyyy")
      if (value.isEmpty) None
      else Some((DateTime.parse(value, format).getMillis / 1000).toString)
    }
  }

  val filename = args.head
  val donations = CSVReader.open(new File(filename)).allWithHeaders

  donations foreach { entry =>
    // benefactor
    val benefactorName = entry("Donor name").string
    val benefactorProperties = propertise(
      "name" -> benefactorName,
      "type" -> entry("Donor type").string,
      "companyRegistrationNumber" -> entry("Company reg. no.").drop(1).string, // optional
      "postcode" -> entry("Postcode").string // optional
    )
    Cypher(s"MERGE (:Benefactor {$benefactorProperties})").execute()

    // recipient
    val recipientName = entry("Entity name").string
    val recipientProperties = propertise(
      "name" -> recipientName,
      "type" -> entry("Entity type").string,
      "regulatedType" -> entry("Regulated donee type").string // optional
    )
    Cypher(s"MERGE (:Recipient {$recipientProperties})").execute()

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
    val matchCypher = s"MATCH (b:Benefactor {name:$benefactorName}), (r:Recipient {name:$recipientName})"
    val mergeCypher = s"MERGE (b)-[:DONATED_TO {$donationProperties}]->(r)"
    Cypher(s"$matchCypher $mergeCypher").execute()

    println(s"Adding donation: ${benefactorName.get} -> ${recipientName.get}")
  }

  def propertise(values: (String, Option[String])*): String = {
    val properties = values collect {
      case (key, Some(value)) => s"$key:$value"
    }
    properties mkString ","
  }

}
