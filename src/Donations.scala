import java.io.File
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import org.anormcypher.Cypher
import com.github.tototoshi.csv.CSVReader
import CypherTools._

object Donations {

  implicit class CypherParameterValue(value: String) {
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
      val benefactor = getBenefactor(entry)
      val benefactorName = benefactor("name").get
      addBenefactor(benefactor)
      val recipient = getRecipient(entry)
      val recipientName = recipient("name").get
      addRecipient(recipient)
      val donation = getDonation(entry)
      addDonation(donation, benefactorName, recipientName)
      println(s"Adding donation: $benefactorName -> $recipientName")
    }
  }

  def getBenefactor(entry: Map[String, String]): Map[String, Option[String]] = {
    Map(
      "name" -> stripTitles(clean(entry("Donor name"))).string,
      "benefactorType" -> clean(entry("Donor type")).string,
      "postcode" -> clean(entry("Postcode")).string, // optional
      "companyNumber" -> clean(entry("Company reg. no.").replaceAll("[^0+A-Za-z0-9]", "").replaceAll("^0*", "")).string // optional
    )
  }

  def getRecipient(entry: Map[String, String]): Map[String, Option[String]] = {
    Map(
      "name" -> stripTitles(clean(entry("Entity name"))).string,
      "recipientType" -> clean(entry("Entity type")).string,
      "recipientRegulatedType" -> clean(entry("Regulated donee type")).string // optional
    )
  }

  def getDonation(entry: Map[String, String]): Map[String, Option[String]] = {
    Map(
      "ecReference" -> clean(entry("EC reference")).string,
      "type" -> clean(entry("Type of donation")).string,
      "value" -> clean(entry("Value")).int, // in pence
      "acceptedDate" -> clean(entry("Accepted date")).date,
      "receivedDate" -> clean(entry("Received date")).date, // optional
      "reportedDate" -> clean(entry("Reported date")).date, // optional
      "nature" -> clean(entry("Nature / Provision")).string, // optional
      "purpose" -> clean(entry("Purpose")).string, // optional
      "howDealtWith" -> clean(entry("How dealt with")).string, // optional
      "recordedBy" -> clean(entry("Rec'd by (AU)")).string, // optional
      "reportedUnder6212" -> clean(entry("Reported under 62:12")).string, // optional
      "isSponsorship" -> clean(entry("Is sponsorship")).boolean,
      "complianceBreach" -> clean(entry("Compliance breach")).string
    )
  }

  def addBenefactor(benefactor: Map[String, Option[String]]): Unit = {
    val properties = benefactor.propertise()
    val companyNumber = benefactor("companyNumber")
    val nodeType = if (benefactor("benefactorType").get contains "Individual") "Individual" else "Organisation"
    if (companyNumber.isEmpty || Cypher(s"MATCH (c {companyNumber:${companyNumber.get}}) RETURN c").apply().isEmpty) {
      val result = Cypher(s"MERGE (:$nodeType {$properties})").execute()
      if (!result) println(" => failed to add benefactor")
    }
  }

  def addRecipient(recipient: Map[String, Option[String]]): Unit = {
    val properties = recipient.propertise()
    val nodeType = "`" + recipient("recipientType").get.tail.init + "`"
    val result = Cypher(s"MERGE (:$nodeType {$properties})").execute()
    if (!result) println(" => failed to add recipient")
  }

  def addDonation(donation: Map[String, Option[String]], benefactorName: String, recipientName: String): Unit = {
    val properties = donation.propertise()
    val matchCypher = s"MATCH (b {name:$benefactorName}), (r {name:$recipientName})"
    val createCypher = s"CREATE (b)-[:DONATED_TO {$properties}]->(r)"
    val result = Cypher(s"$matchCypher $createCypher").execute()
    if (!result) println(" => failed to add donation")
  }

  def clean(text: String): String = {
    text.filter(_ >= ' ').replace("""\""", """\\""").replace("'", """\'""").trim
  }

  def stripTitles(name: String): String = {
    val prefixes = List("Ms", "Mrs", "Miss", "Mr", "Dr", "Lord", "Baron", "Baroness", "Cllr", "Sir", "Dame", "The Hon", "The Rt Hon")
    val suffixes = List("QC", "MP", "MSP", "AM")
    val titlesRegex = (prefixes.map("(" + _ + " )") ++ suffixes.map("( " + _ + ")")).mkString("|")
    name.replaceAll(titlesRegex, "")
  }

}
