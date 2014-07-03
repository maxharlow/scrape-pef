import java.io.File
import scala.collection.immutable.ListMap
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
    val newDonations = CSVWriter.open(new File(file.getPath.stripSuffix(".csv") + "-clean.csv"))
    newDonations.writeRow(newDonationEntries.head.keySet.toSeq)
    newDonationEntries map { entry =>
      newDonations.writeRow(entry.values.toSeq)
    }
    newDonations.close()
  }

  private def getBenefactor(entry: Map[String, String]): Map[String, String] = {
    ListMap(
      "benefactorClass" -> {
        val benefactorType = entry("Donor type")
        if (benefactorType == "Individual" || benefactorType == "Permitted Participant") "Individual"
        else "Organisation"
      },
      "benefactorName" -> {
        val name = entry("Donor name")
        if (entry("Donor type") == "Individual") stripTitles(name)
        else if (!name.contains(", ")) name
        else name.split(", ").head // split from address
      },
      "benefactorType" -> entry("Donor type"),
      "benefactorAddress" -> {
        val name = entry("Donor name")
        if (entry("Donor type") == "Individual") ""
        else if (!name.contains(", ")) ""
        else name.split(", ").tail.mkString(", ").replaceAll("^(A)$|^(NA)$", "") // split from name
      },
      "benefactorPostcode" -> stripFakePostcodes(entry("Postcode")), // optional
      "benefactorCompanyNumber" -> entry("Company reg. no.").replaceAll("[^0+A-Za-z0-9]", "").replaceAll("^0*", "") // optional
    )
  }

  private def getRecipient(entry: Map[String, String]): Map[String, String] = {
    ListMap(
      "recipientClass" -> {
        val recipientType = entry("Entity type")
        val recipientRegulatedType = entry("Regulated donee type")
        if (recipientType == "Political Party" || recipientType == "Third Party") "Party"
        else if (recipientRegulatedType == "Members Association" || recipientRegulatedType == "Permitted Participant") "Organisation"
        else "Individual"
      },
      "recipientName" -> stripTitles(entry("Entity name")).replaceAll("Conservative and Unionist Party", "Conservative Party"),
      "recipientType" -> entry("Entity type"),
      "recipientRegulatedType" -> entry("Regulated donee type") // optional
    )
  }

  private def getDonation(entry: Map[String, String]): Map[String, String] = {
    ListMap(
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
