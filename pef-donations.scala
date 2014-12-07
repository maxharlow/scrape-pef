import scala.util.Try
import scala.collection.immutable.ListMap
import com.gargoylesoftware.htmlunit.html._

object Donations extends PEF {

  run("pef-donations.csv")

  val headers = List(
    "benefactorClass",
    "benefactorName",
    "benefactorType",
    "benefactorAddress",
    "benefactorPostcode",
    "benefactorCompanyNumber",
    "recipientClass",
    "recipientName",
    "recipientType",
    "recipientRegulatedType",
    "recipientDeregisteredDate",
    "ecReference",
    "type",
    "value",
    "acceptedDate",
    "receivedDate",
    "reportedDate",
    "nature",
    "purpose",
    "explanatoryNotes",
    "howDealtWith",
    "recordedBy",
    "reportedUnder6212",
    "isSponsorship"
  )

  val controlSearch = "ctl00$ctl05$ctl01"
  val controlResult = "ctl00_ContentPlaceHolder1_searchControl1_grdDonationFullResults_ctl00_ctl04_lbViewDonationReturnItem"

  override def lookupList(response: HtmlPage): Map[String, String] = {
    ListMap(
      //      "ecReference" -> r.getElementById[HtmlSpan]("ctl00_ContentPlaceHolder1_DonationControl1_lblRefValue", false).getTextContent(),
      "explanatoryNotes" -> Try(response.getElementByName[HtmlTextArea]("ctl00$ContentPlaceHolder1$DonationControl1$txtExplanatoryNotes")).map(_.getTextContent()).getOrElse("")
    )
  }

  override def select(record: Map[String, String]): Map[String, String] = {
    ListMap(
      "benefactorClass" -> {
        val benefactorType = record("Donor type")
        if (benefactorType == "Individual" || benefactorType == "Permitted Participant") "Individual"
        else if (benefactorType == "Registered Political Party") "Party"
        else "Organisation"
      },
      "benefactorName" -> {
        val name = record("Donor name").replaceAll(", ([0-9]),", ", $1").replaceAll(" - Sponsorship", "")
        if (record("Donor type") == "Individual") stripTitles(name)
        else if (name contains ", ") name.split(", ").init.mkString(", ") // split from address
        else name
      },
      "benefactorType" -> record("Donor type"),
      "benefactorAddress" -> {
        val name = record("Donor name").replaceAll(", ([0-9]),", ", $1")
        if (record("Donor type") == "Individual") ""
        else if (name contains ", ") name.split(", ").last.replaceAll("^(A)$|^(NA)$", "") // split from name
        else ""
      },
      "benefactorPostcode" -> stripFakePostcodes(record("Postcode")), // optional
      "benefactorCompanyNumber" -> {
        val benefactorType = record("Donor type")
        if (benefactorType == "Registered Political Party") "" // parties shouldn't have company numbers
        else record("Company reg. no.").replaceAll("[^0+A-Za-z0-9]", "").replaceAll("^0*", "") // optional
      },
      "recipientClass" -> {
        val recipientType = record("Entity type")
        val recipientRegulatedType = record("Regulated donee type")
        if (recipientType == "Political Party" || recipientType == "Third Party") "Party"
        else if (recipientRegulatedType == "Members Association" || recipientRegulatedType == "Permitted Participant") "Organisation"
        else "Individual"
      },
      "recipientName" -> stripTitles(record("Entity name")).replaceAll(""" \[De-registered .*\]""", "").replaceAll("Conservative and Unionist Party", "Conservative Party"),
      "recipientType" -> record("Entity type"),
      "recipientRegulatedType" -> record("Regulated donee type"), // optional
      "recipientDeregisteredDate" -> { // optional, de-registered parties only
        val recipientName = record("Entity name")
        if (recipientName contains "De-registered") asDate(recipientName.replaceAll(".*De-registered ", ""), "dd/MM/yy]")
        else ""
      },
      "ecReference" -> record("EC reference"),
      "type" -> record("Type of donation"),
      "value" -> record("Value").replaceAll("[^0-9]", ""), // in pence
      "acceptedDate" -> asDate(record("Accepted date"), "dd/MM/yyyy"),
      "receivedDate" -> asDate(record("Received date"), "dd/MM/yyyy"), // optional
      "reportedDate" -> asDate(record("Reported date"), "dd/MM/yyyy"), // optional
      "nature" -> record("Nature / Provision"), // optional
      "purpose" -> record("Purpose"), // optional
      "explanatoryNotes" -> record("explanatoryNotes"), // optional
      "howDealtWith" -> record("How dealt with"), // optional
      "recordedBy" -> record("Rec'd by (AU)"), // optional
      "reportedUnder6212" -> asBoolean(record("Reported under 62:12")), // optional
      "isSponsorship" -> asBoolean(record("Is sponsorship"))
    )
  }

}
