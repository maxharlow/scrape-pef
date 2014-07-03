import java.io.File
import com.github.tototoshi.csv.CSVReader
import org.anormcypher.Cypher
import CypherTools._

class Loans(file: File) {

  def loadFile(): Unit = {
    val loans = CSVReader.open(file).allWithHeaders
    for (entry <- loans) {
      val benefactor = getBenefactor(entry)
      val benefactorName = benefactor.values("name").get
      val recipient = getRecipient(entry)
      val recipientName = recipient.values("name").get
      val loan = getLoan(entry)
      addBenefactor(benefactor)
      addRecipient(recipient)
      addLoan(loan, benefactorName, recipientName)
      println(s"Added loan: $benefactorName -> $recipientName")
    }
  }

  private def getBenefactor(entry: Map[String, String]): CypherObject = {
    new CypherObject(entry("benefactorClass"))(
      "name" -> entry("benefactorName").string,
      "benefactorType" -> entry("benefactorType").string,
      "postcode" -> entry("benefactorPostcode").string, // optional
      "companyNumber" -> entry("benefactorCompanyNumber").string // optional
    )
  }

  private def getRecipient(entry: Map[String, String]): CypherObject = {
    new CypherObject(entry("recipientClass"))(
      "name" -> entry("recipientName").string,
      "recipientType" -> entry("recipientType").string
    )
  }

  private def getLoan(entry: Map[String, String]): CypherObject = {
    new CypherObject("LOANED")(
      "ecReference" -> entry("ecReference").string,
      "type" -> entry("type").string,
      "value" -> entry("value").dropRight(2).int, // in pence (to four decimal places...?)
      "referenceNumber" -> entry("referenceNumber").string, // optional
      "rate" -> entry("rate").string, // optional
      "status" -> entry("status").string,
      "amountRepaid" -> entry("amountRepaid").int,
      "amountConverted" -> entry("amountConverted").int,
      "amountOutstanding" -> entry("amountOutstanding").int,
      "startDate" -> entry("startDate").date("yyyy-MM-dd"),
      "endDate" -> entry("endDate").date("yyyy-MM-dd"), // optional
      "repaidDate" -> entry("repaidDate").date("yyyy-MM-dd"), // optional
      "ecLastNotifiedDate" -> entry("ecLastNotifiedDate").date("yyyy-MM-dd"),
      "recordedBy" -> entry("recordedBy").string // optional
    )
  }

  private def addBenefactor(benefactor: CypherObject): Unit = {
    val companyNumber = benefactor.values("companyNumber")
    if (companyNumber.isEmpty || Cypher(s"MATCH (c {companyNumber:${companyNumber.get}}) RETURN c").apply().isEmpty) {
      val benefactorName = benefactor.values("name").get
      val result = if (Cypher(s"MATCH b WHERE b.name = $benefactorName RETURN b").apply().isEmpty) {
        val benefactorProperties = benefactor.toMatchString()
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
    val recipientName = recipient.values("name").get
    val result = if (Cypher(s"MATCH r WHERE r.name = $recipientName RETURN r").apply().isEmpty) {
      val recipientProperties = recipient.toMatchString()
      Cypher(s"CREATE ($recipientProperties)").execute()
    }
    else { // recipient already exists
      val recipientProperties = recipient.toUpdateString("r")
      Cypher(s"MATCH r WHERE r.name = $recipientName SET $recipientProperties").execute()
    }
    if (!result) println(" => failed to add recipient")
  }

  private def addLoan(loan: CypherObject, benefactorName: String, recipientName: String): Unit = {
    val loanProperties = loan.toMatchString()
    val matchCypher = s"MATCH (b {name:$benefactorName}), (r {name:$recipientName})"
    val createCypher = s"CREATE (b)-[$loanProperties]->(r)"
    val result = Cypher(s"$matchCypher $createCypher").execute()
    if (!result) println(" => failed to add loan")
  }

}
