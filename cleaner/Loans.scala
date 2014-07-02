import java.io.File
import com.github.tototoshi.csv.{CSVReader, CSVWriter}
import TextTools._

class Loans(file: File) {

  def cleanFile(): Unit = {
    val loans = CSVReader.open(file)
    val newLoanEntries = loans.allWithHeaders map { entry =>
      val loan = getLoan(entry.mapValues(clean))
      val benefactor = getBenefactor(entry.mapValues(clean))
      val recipient = getRecipient(entry.mapValues(clean))
      loan ++ benefactor ++ recipient
    }
    loans.close()
    val newLoans = CSVWriter.open(file)
    newLoans.writeRow(newLoanEntries.head.keySet.toSeq)
    newLoanEntries map { entry =>
      newLoans.writeRow(entry.values.toSeq)
    }
    newLoans.close()
  }

  private def getBenefactor(entry: Map[String, String]): Map[String, String] = {
    Map(
      "benefactorName" -> (if (entry("Lender type") == "Individual") stripTitles(entry("Lender name")) else entry("Lender name")),
      "benefactorType" -> entry("Lender type"),
      "benefactorPostcode" -> stripFakePostcodes(entry("Postcode")), // optional
      "benefactorCompanyNumber" -> entry("Company reg. no.").replaceAll("[^0+A-Za-z0-9]", "").replaceAll("^0*", "") // optional
    )
  }

  private def getRecipient(entry: Map[String, String]): Map[String, String] = {
    Map(
      "recipientName" -> stripTitles(entry("Entity name")),
      "recipientType" -> entry("Entity type")
    )
  }

  private def getLoan(entry: Map[String, String]): Map[String, String] = {
    Map(
      "ecReference" -> entry("EC reference"),
      "type" -> entry("Type of borrowing"),
      "value" -> asInt(entry("Total amount").dropRight(2)), // in pence (to four decimal places...?)
      "referenceNumber" -> entry("Loan reference no."), // optional
      "rate" -> entry("Rate"), // optional
      "status" -> entry("Status"),
      "amountRepaid" -> asInt(entry("Amount repaid").dropRight(2)),
      "amountConverted" -> asInt(entry("Amount converted").dropRight(2)),
      "amountOutstanding" -> asInt(entry("Amount outstanding").dropRight(2)),
      "startDate" -> asDate(entry("Start date"), "dd/MM/yyyy"),
      "endDate" -> asDate(entry("End date"), "dd/MM/yyyy"), // optional
      "repaidDate" -> asDate(entry("Date repaid"), "dd/MM/yyyy"), // optional
      "ecLastNotifiedDate" -> asDate(entry("Date EC last notified"), "dd/MM/yyyy"),
      "recordedBy" -> entry("Rec'd by (AU)"), // optional
      "complianceBreach" -> entry("Compliance breach")
    )
  }

}
