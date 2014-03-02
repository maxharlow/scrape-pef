import java.io.File
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import org.anormcypher.Cypher
import com.github.tototoshi.csv.CSVReader

object Grapher extends App {

  implicit class CypherParameterValue(v: String) {
    val value = v.trim.filter(_ >= ' ')
    def int = Option(value.replaceAll("[^0-9]", "").dropRight(2)).filter(_.nonEmpty) // inexplicably values here are given to four decimal places
    def string = if (value.isEmpty) None else Some("'" + value + "'")
    def boolean = Some((!value.isEmpty).toString)
    def date = {
      val format = DateTimeFormat.forPattern("dd/MM/yyyy")
      if (value.isEmpty) None
      else Some((DateTime.parse(value, format).getMillis / 1000).toString)
    }
  }

  val filename = args.head
  val loans = CSVReader.open(new File(filename)).allWithHeaders

  loans foreach { entry =>
    // benefactor
    val benefactorName = entry("Lender name").string
    val benefactorProperties = propertise(
      "name" -> benefactorName,
      "type" -> entry("Lender type").string,
      "companyRegistrationNumber" -> entry("Company reg. no.").drop(1).string, // optional
      "postcode" -> entry("Postcode").string // optional
    )
    Cypher(s"MERGE (:Benefactor {$benefactorProperties})").execute()

    // recipient
    val recipientName = entry("Entity name").string
    val recipientProperties = propertise(
      "name" -> recipientName,
      "type" -> entry("Entity type").string
    )
    Cypher(s"MERGE (:Recipient {$recipientProperties})").execute()

    // loan
    val loanProperties = propertise(
      "ecReference" -> entry("EC reference").string,
      "type" -> entry("Type of borrowing").string,
      "value" -> entry("Total amount").int, // in pence
      "referenceNumber" -> entry("Loan reference no.").string, // optional
      "rate" -> entry("Rate").string, // optional
      "status" -> entry("Status").string,
      "amountRepaid" -> entry("Amount repaid").int,
      "amountConverted" -> entry("Amount converted").int,
      "amountOutstanding" -> entry("Amount outstanding").int,
      "startDate" -> entry("Start date").date,
      "endDate" -> entry("End date").date, // optional
      "repaidDate" -> entry("Date repaid").date, // optional
      "ecLastNotifiedDate" -> entry("Date EC last notified").date,
      "recordedBy" -> entry("Rec'd by (AU)").string, // optional
      "complianceBreach" -> entry("Compliance breach").string
    )
    val matchCypher = s"MATCH (b:Benefactor {name:$benefactorName}), (r:Recipient {name:$recipientName})"
    val mergeCypher = s"MERGE (b)-[:LOANED {$loanProperties}]->(r)"
    Cypher(s"$matchCypher $mergeCypher").execute()

    println(s"Adding loan: ${benefactorName.get} -> ${recipientName.get}")
  }


  def propertise(values: (String, Option[String])*): String = {
    val properties = values collect {
      case (key, Some(value)) => s"$key:$value"
    }
    properties mkString ","
  }

}
