import java.io.File
import com.github.tototoshi.csv.{CSVReader, CSVWriter}
import TextTools._

class Donations(file: File) {

  def cleanFile(): Unit = {
    val donations = CSVReader.open(file)
    val newDonationEntries = donations.allWithHeaders map { entry =>
      val donation = getDonation(entry.mapValues(clean))
      val benefactor = getBenefactor(entry.mapValues(clean))
      val recipient = getRecipient(entry.mapValues(clean))
      donation ++ benefactor ++ recipient
    }
    donations.close()
    val newDonations = CSVWriter.open(file)
    newDonations.writeRow(newDonationEntries.head.keySet.toSeq)
    newDonationEntries map { entry =>
      newDonations.writeRow(entry.values.toSeq)
    }
    newDonations.close()
  }

  private def getBenefactor(entry: Map[String, String]): Map[String, String] = {
    Map(
      "benefactorName" -> (if (entry("Donor type") == "Individual") stripTitles(entry("Donor name")) else entry("Donor name")),
      "benefactorType" -> entry("Donor type"),
      "benefactorPostcode" -> entry("Postcode"), // optional
      "benefactorCompanyNumber" -> entry("Company reg. no.").replaceAll("[^0+A-Za-z0-9]", "").replaceAll("^0*", "") // optional
    )
  }

  private def getRecipient(entry: Map[String, String]): Map[String, String] = {
    Map(
      "recipientName" -> stripTitles(entry("Entity name")),
      "recipientType" -> entry("Entity type"),
      "recipientRegulatedType" -> entry("Regulated donee type") // optional
    )
  }

  private def getDonation(entry: Map[String, String]): Map[String, String] = {
    Map(
      "ecReference" -> entry("EC reference"),
      "type" -> entry("Type of donation"),
      "value" -> entry("Value").replaceAll("[^0-9]", ""), // in pence
      "acceptedDate" -> asDate(entry("Accepted date"), "dd/MM/yyyy"),
      "receivedDate" -> asDate(entry("Received date"), "dd/MM/yyyy"), // optional
      "reportedDate" -> asDate(entry("Reported date"), "dd/MM/yyyy"), // optional
      "nature" -> entry("Nature / Provision"), // optional
      "purpose" -> entry("Purpose"), // optional
      "howDealtWith" -> entry("How dealt with"), // optional
      "recordedBy" -> entry("Rec'd by (AU)"), // optional
      "reportedUnder6212" -> asBoolean(entry("Reported under 62:12")), // optional
      "isSponsorship" -> asBoolean(entry("Is sponsorship"))
    )
  }

}
