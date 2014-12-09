import scala.util.Try
import scala.collection.immutable.ListMap
import scala.collection.JavaConversions._
import com.gargoylesoftware.htmlunit.html._

object Donations extends PEF {

  override val headers = List(
    "ecReference",
    "ecReportedDate",
    "ecPublishedDate",
    "ecReleaseTitle",
    "donorType",
    "donorTitle",
    "donorFirstName",
    "donorMiddleName",
    "donorLastName",
    "donorName",
    "donorCompanyNumber",
    "donorAddress",
    "donorPostcode",
    "donorCountry",
    "donorPhoneNumber",
    "donorEmailAddress",
    "recipientName",
    "recipientID",
    "recipientType",
    "recipientRegulatedType",
    "recipientAccountingUnit",
    "recipientAccountingUnitID",
    "recipientDeregisteredDate",
    "value",
    "type",
    "receivedDate",
    "acceptedDate",
    "returnedDate",
    "nature",
    "purpose",
    "notes",
    "howDealtWith",
    "isReportedDueToAggregation",
    "isReportedUnder6212",
    "isSponsorship"
  )

  override val controlSearch = "ctl00$ctl05$ctl01"
  override val controlResult = "ctl00_ContentPlaceHolder1_searchControl1_grdDonationFullResults_ctl00_ctl04_lbViewDonationReturnItem"

  run("pef-donations.csv")

  override def select(record: Map[String, String], page: HtmlPage): Map[String, String] = {
    ListMap(
      "ecReference" -> record("EC reference"),
      "ecReportedDate" -> asDate(record("Reported date"), "dd/MM/yyyy"), // aka 'submitted date'
      "ecPublishedDate" -> {
        val publishedDate = Try(page.getElementById[HtmlSpan]("ctl00_ContentPlaceHolder1_DonationControl1_lblPublishedDateValue", true))        
        publishedDate.map(_.getTextContent()).map(asDate(_, "dd/MM/yyyy")).getOrElse("")
      },
      "ecReleaseTitle" -> page.getElementById[HtmlSpan]("ctl00_ContentPlaceHolder1_DonationControl1_lblDonationTitle", true).getTextContent(),
      "donorType" -> record("Donor type"),
      "donorTitle" -> { // individuals only
        val title = Try(page.getElementByName[HtmlSelect]("ctl00$ContentPlaceHolder1$DonationControl1$participant1$ddlTitle"))
        val titleText = title.map(_.getSelectedOptions().head.getTextContent()).getOrElse("")
        if (titleText matches "na|-- Please Select --") "" else titleText
      },
      "donorFirstName" -> { // individuals only
        val firstName = Try(page.getElementByName[HtmlInput]("ctl00$ContentPlaceHolder1$DonationControl1$participant1$txtFirstname"))
        val firstNameText = firstName.map(_.getValueAttribute()).getOrElse("")
        if (firstNameText == "na") "" else firstNameText
      },
      "donorMiddleName" -> { // individuals only
        val middleName = Try(page.getElementByName[HtmlInput]("ctl00$ContentPlaceHolder1$DonationControl1$participant1$txtMiddlename"))
        val middleNameText = middleName.map(_.getValueAttribute()).getOrElse("")
        if (middleNameText == "na") "" else middleNameText
      },
      "donorLastName" -> { // individuals only
        val lastName = Try(page.getElementByName[HtmlInput]("ctl00$ContentPlaceHolder1$DonationControl1$participant1$txtSurname"))
        val lastNameText = lastName.map(_.getValueAttribute()).getOrElse("")
        if (lastNameText == "na") "" else lastNameText
      },
      "donorName" -> { // non-individuals only
        val donorName = Try(page.getElementByName[HtmlInput]("ctl00$ContentPlaceHolder1$DonationControl1$participant1$txtParticupantName"))
        donorName.map(_.getValueAttribute()).getOrElse("")
      },
      "donorCompanyNumber" -> { // companies only
        val donorCompanyNumber = Try(page.getElementByName[HtmlInput]("ctl00$ContentPlaceHolder1$DonationControl1$participant1$txtCompanyRegistrationNumber"))
        donorCompanyNumber.map(_.getValueAttribute()).getOrElse("")
      },
      "donorAddress" -> {
        val address = List(
          Try(page.getElementByName[HtmlInput]("ctl00$ContentPlaceHolder1$DonationControl1$participant1$txtAddressLine1")).map(_.getValueAttribute()),
          Try(page.getElementByName[HtmlInput]("ctl00$ContentPlaceHolder1$DonationControl1$participant1$txtAddressLine2")).map(_.getValueAttribute()),
          Try(page.getElementByName[HtmlInput]("ctl00$ContentPlaceHolder1$DonationControl1$participant1$txtAddressLine3")).map(_.getValueAttribute()),
          Try(page.getElementByName[HtmlInput]("ctl00$ContentPlaceHolder1$DonationControl1$participant1$txtAddressLine4")).map(_.getValueAttribute()),
          Try(page.getElementByName[HtmlInput]("ctl00$ContentPlaceHolder1$DonationControl1$participant1$txtTown")).map(_.getValueAttribute()).map(_.replace("no town", "")),
          Try(page.getElementByName[HtmlSelect]("ctl00$ContentPlaceHolder1$DonationControl1$participant1$ddlCounty")).map(_.getSelectedOptions().head.getTextContent())
        )
        address.map(_.getOrElse("")).filterNot(_.isEmpty).mkString(", ")
      },
      "donorPostcode" -> {
        val postcode = Try(page.getElementByName[HtmlInput]("ctl00$ContentPlaceHolder1$DonationControl1$participant1$txtPostcode"))
        postcode.map(_.getValueAttribute()).map(stripFakePostcodes).getOrElse("")
      },
      "donorCountry" -> {
        val donorCountry = Try(page.getElementByName[HtmlSelect]("ctl00$ContentPlaceHolder1$DonationControl1$participant1$ddlCountry"))
        donorCountry.map(_.getSelectedOptions().head.getTextContent()).getOrElse("")
      },
      "donorPhoneNumber" -> {
        val donorPhoneNumber = Try(page.getElementByName[HtmlInput]("ctl00$ContentPlaceHolder1$DonationControl1$participant1$txtPhoneNumber"))
        donorPhoneNumber.map(_.getValueAttribute()).getOrElse("")
      },
      "donorEmailAddress" -> {
        val donorEmailAddress = Try(page.getElementByName[HtmlInput]("ctl00$ContentPlaceHolder1$DonationControl1$participant1$txtEmail"))
        donorEmailAddress.map(_.getValueAttribute()).getOrElse("")
      },
      "recipientName" -> {
        val recipientName = stripTitles(record("Entity name")).replaceAll(""" \[De-registered .*\]""", "")
        recipientName.replaceAll("Conservative and Unionist Party", "Conservative Party")
      },
      "recipientID" -> record("Entity ID"),
      "recipientType" -> record("Entity type"),
      "recipientRegulatedType" -> record("Regulated donee type"),
      "recipientAccountingUnit" -> { // aka 'recorded by' or 'received by'
        val accountingUnit = Try(page.getElementByName[HtmlSelect]("ctl00$ContentPlaceHolder1$DonationControl1$ddlReceivedBy"))
        accountingUnit.map(_.getSelectedOptions().head.getTextContent()).getOrElse("")
      },
      "recipientAccountingUnitID" -> record("Accounting unit ID"),
      "recipientDeregisteredDate" -> { // for de-registered parties
        val recipientName = record("Entity name")
        if (recipientName contains "De-registered") asDate(recipientName.replaceAll(".*De-registered ", ""), "dd/MM/yy]")
        else ""
      },
      "value" -> record("Value"),
      "type" -> record("Type of donation"),
      "receivedDate" -> asDate(record("Received date"), "dd/MM/yyyy"),
      "acceptedDate" -> asDate(record("Accepted date"), "dd/MM/yyyy"),
      "returnedDate" -> { // for impermissable donations
        val year = Try(page.getElementByName[HtmlSelect]("ctl00$ContentPlaceHolder1$DonationControl1$dsdateReturned$ddlYear")).map(_.getTextContent()).getOrElse("")
        val month = Try(page.getElementByName[HtmlSelect]("ctl00$ContentPlaceHolder1$DonationControl1$dsdateReturned$ddlMonth")).map(_.getTextContent()).getOrElse("")
        val day = Try(page.getElementByName[HtmlSelect]("ctl00$ContentPlaceHolder1$DonationControl1$dsdateReturned$ddlDay")).map(_.getTextContent()).getOrElse("")
        val date = s"$year-$month-$day"
        if (date == "--") "" else date
      },
      "nature" -> record("Nature / Provision"),
      "purpose" -> record("Purpose"),
      "notes" -> {
        val explanatoryNotes = Try(page.getElementByName[HtmlTextArea]("ctl00$ContentPlaceHolder1$DonationControl1$txtExplanatoryNotes"))
        explanatoryNotes.map(_.getTextContent()).getOrElse("")
      },
      "howDealtWith" -> record("How dealt with"),
      "isReportedDueToAggregation" -> {
        val isReportedDueToAggregation = Try(page.getElementByName[HtmlCheckBoxInput]("ctl00$ContentPlaceHolder1$DonationControl1$chkAggregation"))
        isReportedDueToAggregation.map(_.isChecked()).getOrElse(false).toString()
      },
      "isReportedUnder6212" -> asBoolean(record("Reported under 62:12")),
      "isSponsorship" -> asBoolean(record("Is sponsorship"))
    )
  }

}
