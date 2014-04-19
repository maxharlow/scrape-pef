import java.io.File
import scala.collection.JavaConversions._
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import org.anormcypher.Cypher
import com.github.tototoshi.csv.CSVReader
import CypherTools._

object Donations {

  def run(periodStartDate: DateTime, periodEndDate: DateTime) {
    val files = new File(Config.dataLocation).listFiles
    for (file <- files if file.getName matches "(donations-)\\d{4}.*(.csv)") {
      val donations = CSVReader.open(file).allWithHeaders
      for (entry <- donations) {
        val benefactor = getBenefactor(entry)
        val benefactorName = benefactor.values("name").get
        val recipient = getRecipient(entry)
        val recipientName = recipient.values("name").get
        val donation = getDonation(entry)
        val donationAcceptedDate = DateTime.parse(donation.values("acceptedDate").get, DateTimeFormat.forPattern("yyyyMMdd"))
        if ((donationAcceptedDate isAfter periodStartDate) && (donationAcceptedDate isBefore periodEndDate)) {
          addBenefactor(benefactor)
          addRecipient(recipient)
          addDonation(donation, benefactorName, recipientName)
          println(s"Added donation: $benefactorName -> $recipientName")
        }
      }
    }
  }

  private def getBenefactor(entry: Map[String, String]): CypherObject = {
    val name = clean(entry("Donor name"))
    new CypherObject(
      "name" -> (if (entry("Donor type") == "Individual") stripTitles(name) else name).string,
      "benefactorType" -> clean(entry("Donor type")).string,
      "postcode" -> clean(entry("Postcode")).string, // optional
      "companyNumber" -> clean(entry("Company reg. no.").replaceAll("[^0+A-Za-z0-9]", "").replaceAll("^0*", "")).string // optional
    )
  }

  private def getRecipient(entry: Map[String, String]): CypherObject = {
    new CypherObject(
      "name" -> stripTitles(clean(entry("Entity name"))).string,
      "recipientType" -> clean(entry("Entity type")).string,
      "recipientRegulatedType" -> clean(entry("Regulated donee type")).string // optional
    )
  }

  private def getDonation(entry: Map[String, String]): CypherObject = {
    new CypherObject(
      "ecReference" -> clean(entry("EC reference")).string,
      "type" -> clean(entry("Type of donation")).string,
      "value" -> clean(entry("Value")).int, // in pence
      "acceptedDate" -> clean(entry("Accepted date")).date("dd/MM/yyyy"),
      "receivedDate" -> clean(entry("Received date")).date("dd/MM/yyyy"), // optional
      "reportedDate" -> clean(entry("Reported date")).date("dd/MM/yyyy"), // optional
      "nature" -> clean(entry("Nature / Provision")).string, // optional
      "purpose" -> clean(entry("Purpose")).string, // optional
      "howDealtWith" -> clean(entry("How dealt with")).string, // optional
      "recordedBy" -> clean(entry("Rec'd by (AU)")).string, // optional
      "reportedUnder6212" -> clean(entry("Reported under 62:12")).string, // optional
      "isSponsorship" -> clean(entry("Is sponsorship")).boolean,
      "complianceBreach" -> clean(entry("Compliance breach")).string
    )
  }

  private def addBenefactor(benefactor: CypherObject): Unit = {
    val companyNumber = benefactor.values("companyNumber")
    if (companyNumber.isEmpty || Cypher(s"MATCH (c {companyNumber:${companyNumber.get}}) RETURN c").apply().isEmpty) {
      val nodeType = {
        val benefactorType = benefactor.values("benefactorType")
        if (benefactorType == Some("'Individual'") || benefactorType == Some("'Permitted Participant'")) "Individual"
        else "Organisation"
      }
      val benefactorName = benefactor.values("name").get
      val result = if (Cypher(s"MATCH b WHERE b.name = $benefactorName RETURN b").apply().isEmpty) {
        val benefactorProperties = benefactor.toMatchString(nodeType)
        Cypher(s"CREATE ($benefactorProperties)").execute()
      }
      else { // benefactor already exists
        val benefactorProperties = benefactor.toUpdateString("b")
        Cypher(s"MATCH b WHERE b.name = $benefactorName SET $benefactorProperties").execute()
      }
      if (!result) println(" => failed to add benefactor")
    }
  }

  private def addRecipient(recipient: CypherObject): Unit = {
    val nodeType = {
      val recipientType = recipient.values("recipientType")
      if (recipientType == Some("'Political Party'") || recipientType == Some("'Third Party'")) "PoliticalParty"
      else if (recipient.values("recipientRegulatedType") == Some("'Members Association'")) "Organisation"
      else "Individual"
    }
    val recipientName = recipient.values("name").get
    val result = if (Cypher(s"MATCH r WHERE r.name = $recipientName RETURN r").apply().isEmpty) {
      val recipientProperties = recipient.toMatchString(nodeType)
      Cypher(s"CREATE ($recipientProperties)").execute()
    }
    else { // recipient already exists
      val recipientProperties = recipient.toUpdateString("r")
      Cypher(s"MATCH r WHERE r.name = $recipientName SET $recipientProperties").execute()
    }
    if (!result) println(" => failed to add recipient")
  }

  private def addDonation(donation: CypherObject, benefactorName: String, recipientName: String): Unit = {
    val donationProperties = donation.toMatchString("DONATED_TO")
    val matchCypher = s"MATCH (b {name:$benefactorName}), (r {name:$recipientName})"
    val createCypher = s"CREATE (b)-[$donationProperties]->(r)"
    val result = Cypher(s"$matchCypher $createCypher").execute()
    if (!result) println(" => failed to add donation")
  }

  private def clean(text: String): String = {
    text.filter(_ >= ' ').trim
  }

  private def stripTitles(name: String): String = {
    val prefixes = List("Ms", "Mrs", "Miss", "Mr", "Dr", "Cllr", "Sir", "Dame", "The Hon", "The Rt Hon")
    val suffixes = List("QC", "MP", "MSP", "AM", "MEP")
    val titlesRegex = (prefixes.map("^(" + _ + " )") ++ suffixes.map("( " + _ + ")")).mkString("|")
    name.replaceAll(titlesRegex, "").replaceAll("^(na )|( na)", "")
  }

}
