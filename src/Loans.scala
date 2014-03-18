import java.io.File
import org.anormcypher.Cypher
import com.github.tototoshi.csv.CSVReader
import CypherTools._

object Loans {

  def run(dataLocation: String) {
    val loans = CSVReader.open(new File(dataLocation)).allWithHeaders
    loans foreach { entry =>
      val benefactor = getBenefactor(entry)
      val benefactorName = benefactor.values("name").get
      addBenefactor(benefactor)
      val recipient = getRecipient(entry)
      val recipientName = recipient.values("name").get
      addRecipient(recipient)
      val loan = getLoan(entry)
      addLoan(loan, benefactorName, recipientName)
      println(s"Added loan: $benefactorName -> $recipientName")
    }
  }

  private def getBenefactor(entry: Map[String, String]): CypherObject = {
    val name = clean(entry("Lender name"))
    new CypherObject(
      "name" -> (if (entry("Lender type") == "Individual") stripTitles(name) else name).string,
      "benefactorType" -> clean(entry("Lender type")).string,
      "postcode" -> clean(entry("Postcode")).string, // optional
      "companyNumber" -> clean(entry("Company reg. no.").replaceAll("[^0+A-Za-z0-9]", "").replaceAll("^0*", "")).string // optional
    )
  }

  private def getRecipient(entry: Map[String, String]): CypherObject = {
    new CypherObject(
      "name" -> stripTitles(clean(entry("Entity name"))).string,
      "recipientType" -> clean(entry("Entity type")).string
    )
  }

  private def getLoan(entry: Map[String, String]): CypherObject = {
    new CypherObject(
      "ecReference" -> clean(entry("EC reference")).string,
      "type" -> clean(entry("Type of borrowing")).string,
      "value" -> clean(entry("Total amount")).dropRight(2).int, // in pence (to four decimal places...?)
      "referenceNumber" -> clean(entry("Loan reference no.")).string, // optional
      "rate" -> clean(entry("Rate")).string, // optional
      "status" -> clean(entry("Status")).string,
      "amountRepaid" -> clean(entry("Amount repaid")).dropRight(2).int,
      "amountConverted" -> clean(entry("Amount converted")).dropRight(2).int,
      "amountOutstanding" -> clean(entry("Amount outstanding")).dropRight(2).int,
      "startDate" -> clean(entry("Start date")).date("dd/MM/yyyy"),
      "endDate" -> clean(entry("End date")).date("dd/MM/yyyy"), // optional
      "repaidDate" -> clean(entry("Date repaid")).date("dd/MM/yyyy"), // optional
      "ecLastNotifiedDate" -> clean(entry("Date EC last notified")).date("dd/MM/yyyy"),
      "recordedBy" -> clean(entry("Rec'd by (AU)")).string, // optional
      "complianceBreach" -> clean(entry("Compliance breach")).string
    )
  }

  private def addBenefactor(benefactor: CypherObject): Unit = {
    val companyNumber = benefactor.values("companyNumber")
    if (companyNumber.isEmpty || Cypher(s"MATCH (c {companyNumber:${companyNumber.get}}) RETURN c").apply().isEmpty) {
      val nodeType = if (benefactor.values("benefactorType").get contains "Individual") "Individual" else "Organisation"
      val benefactorProperties = benefactor.toMatchString(nodeType, "c")
      val result = Cypher(s"MERGE ($benefactorProperties)").execute()
      if (!result) println(" => failed to add benefactor")
    }
  }

  private def addRecipient(recipient: CypherObject): Unit = {
    val nodeType = {
      if (recipient.values("recipientType") == Some("Political Party")) "PoliticalParty"
      else "Individual"
    }
    val recipientProperties = recipient.toMatchString(nodeType)
    val result = Cypher(s"MERGE ($recipientProperties)").execute()
    if (!result) println(" => failed to add recipient")
  }

  private def addLoan(loan: CypherObject, benefactorName: String, recipientName: String): Unit = {
    val loanProperties = loan.toMatchString("LOANED")
    val matchCypher = s"MATCH (b {name:$benefactorName}), (r {name:$recipientName})"
    val createCypher = s"CREATE (b)-[$loanProperties]->(r)"
    val result = Cypher(s"$matchCypher $createCypher").execute()
    if (!result) println(" => failed to add loan")
  }

  private def clean(text: String): String = {
    text.filter(_ >= ' ').trim
  }

  private def stripTitles(name: String): String = {
    val prefixes = List("Ms", "Mrs", "Miss", "Mr", "Dr", "Cllr", "Sir", "Dame", "The Hon", "The Rt Hon")
    val suffixes = List("QC", "MP", "MSP", "AM", "MEP")
    val titlesRegex = (prefixes.map("^(" + _ + " )") ++ suffixes.map("( " + _ + ")")).mkString("|")
    name.replaceAll(titlesRegex, "")
  }

}
