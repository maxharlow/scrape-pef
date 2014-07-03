import java.io.File
import com.github.tototoshi.csv.CSVReader
import org.anormcypher.Cypher
import CypherTools._

class Donations(file: File) {

  def loadFile(): Unit = {
    val donations = CSVReader.open(file).allWithHeaders
    for (entry <- donations) {
      val benefactor = getBenefactor(entry)
      val benefactorName = benefactor.values("name").get
      val recipient = getRecipient(entry)
      val recipientName = recipient.values("name").get
      val donation = getDonation(entry)
      addBenefactor(benefactor)
      addRecipient(recipient)
      addDonation(donation, benefactorName, recipientName)
      println(s"Added donation: $benefactorName -> $recipientName")
    }
  }

  private def getBenefactor(entry: Map[String, String]): CypherObject = {
    new CypherObject(
      "name" -> entry("benefactorName").string,
      "benefactorType" -> entry("benefactorType").string,
      "postcode" -> entry("benefactorPostcode").string, // optional
      "companyNumber" -> entry("benefactorCompanyNumber").string //optional
    )
  }

  private def getRecipient(entry: Map[String, String]): CypherObject = {
    new CypherObject(
      "name" -> entry("recipientName").string,
      "recipientType" -> entry("recipientType").string,
      "recipientRegulatedType" -> entry("recipientRegulatedType").string // optional
    )
  }

  private def getDonation(entry: Map[String, String]): CypherObject = {
    new CypherObject(
      "ecReference" -> entry("ecReference").string,
      "type" -> entry("type").string,
      "value" -> entry("value").int, // in pence
      "acceptedDate" -> entry("acceptedDate").date("yyyy-MM-dd"),
      "receivedDate" -> entry("receivedDate").date("yyyy-MM-dd"), // optional
      "reportedDate" -> entry("reportedDate").date("yyyy-MM-dd"), // optional
      "nature" -> entry("nature").string, // optional
      "purpose" -> entry("purpose").string, // optional
      "howDealtWith" -> entry("howDealtWith").string, // optional
      "recordedBy" -> entry("recordedBy").string, // optional
      "reportedUnder6212" -> entry("reportedUnder6212").string, // optional
      "isSponsorship" -> entry("isSponsorship").boolean
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
      val recipientRegulatedType  = recipient.values("recipientRegulatedType")
      if (recipientType == Some("'Political Party'") || recipientType == Some("'Third Party'")) "PoliticalParty"
      else if (recipientRegulatedType == Some("'Members Association'") || recipientRegulatedType == Some("'Permitted Participant'")) "Organisation"
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

}
