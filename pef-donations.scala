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
    "accountingUnit",
    "accountingUnitID",
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
      "ecReleaseTitle" -> { // TODO -- last exception said this doesn't exist? TEMP making this optional -- can't think why it should be though
        val ecReleaseTitle = Try(page.getElementById[HtmlSpan]("ctl00_ContentPlaceHolder1_DonationControl1_lblDonationTitle", true))
        ecReleaseTitle.map(_.getTextContent()).getOrElse("")
      },
      "donorType" -> record("Donor type"),
      "donorTitle" -> { // individuals only
        val donorTitle = Try(page.getElementByName[HtmlSelect]("ctl00$ContentPlaceHolder1$DonationControl1$participant1$ddlTitle"))
        val donorTitleText = donorTitle.map(_.getSelectedOptions().head.getTextContent()).getOrElse("")
        if (donorTitleText matches "na|-- Please Select --") "" else donorTitleText
      },
      "donorFirstName" -> { // individuals only
        val donorFirstName = Try(page.getElementByName[HtmlInput]("ctl00$ContentPlaceHolder1$DonationControl1$participant1$txtFirstname"))
        val donorFirstNameText = donorFirstName.map(_.getValueAttribute()).getOrElse("")
        if (donorFirstNameText == "na") "" else donorFirstNameText
      },
      "donorMiddleName" -> { // individuals only
        val donorMiddleName = Try(page.getElementByName[HtmlInput]("ctl00$ContentPlaceHolder1$DonationControl1$participant1$txtMiddlename"))
        val donorMiddleNameText = donorMiddleName.map(_.getValueAttribute()).getOrElse("")
        if (donorMiddleNameText == "na") "" else donorMiddleNameText
      },
      "donorLastName" -> { // individuals only
        val donorLastName = Try(page.getElementByName[HtmlInput]("ctl00$ContentPlaceHolder1$DonationControl1$participant1$txtSurname"))
        val donorLastNameText = donorLastName.map(_.getValueAttribute()).getOrElse("")
        if (donorLastNameText == "na") "" else donorLastNameText
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
        val donorAddress = List(
          Try(page.getElementByName[HtmlInput]("ctl00$ContentPlaceHolder1$DonationControl1$participant1$txtAddressLine1")).map(_.getValueAttribute()),
          Try(page.getElementByName[HtmlInput]("ctl00$ContentPlaceHolder1$DonationControl1$participant1$txtAddressLine2")).map(_.getValueAttribute()),
          Try(page.getElementByName[HtmlInput]("ctl00$ContentPlaceHolder1$DonationControl1$participant1$txtAddressLine3")).map(_.getValueAttribute()),
          Try(page.getElementByName[HtmlInput]("ctl00$ContentPlaceHolder1$DonationControl1$participant1$txtAddressLine4")).map(_.getValueAttribute()),
          Try(page.getElementByName[HtmlInput]("ctl00$ContentPlaceHolder1$DonationControl1$participant1$txtTown")).map(_.getValueAttribute()).map(_.replace("no town", "")),
          Try(page.getElementByName[HtmlSelect]("ctl00$ContentPlaceHolder1$DonationControl1$participant1$ddlCounty")).map(_.getSelectedOptions().head.getTextContent())
        )
        donorAddress.map(_.getOrElse("")).filterNot(_.isEmpty).mkString(", ")
      },
      "donorPostcode" -> {
        val donorPostcode = Try(page.getElementByName[HtmlInput]("ctl00$ContentPlaceHolder1$DonationControl1$participant1$txtPostcode"))
        donorPostcode.map(_.getValueAttribute()).map(stripFakePostcodes).getOrElse("")
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
      "recipientDeregisteredDate" -> { // for de-registered parties
        val recipientName = record("Entity name")
        if (recipientName contains "De-registered") asDate(recipientName.replaceAll(".*De-registered ", ""), "dd/MM/yy]")
        else ""
      },
      "accountingUnit" -> { // aka 'recorded by' or 'received by'
        val accountingUnit = Try(page.getElementByName[HtmlSelect]("ctl00$ContentPlaceHolder1$DonationControl1$ddlReceivedBy"))
        accountingUnit.map(_.getSelectedOptions().head.getTextContent()).getOrElse("")
      },
      "accountingUnitID" -> record("Accounting unit ID"),
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
        val notes = Try(page.getElementByName[HtmlTextArea]("ctl00$ContentPlaceHolder1$DonationControl1$txtExplanatoryNotes"))
        notes.map(_.getTextContent()).getOrElse("")
      },
      "howDealtWith" -> record("How dealt with"),
      "isReportedDueToAggregation" -> {
        val isReportedDueToAggregation = Try(page.getElementByName[HtmlCheckBoxInput]("ctl00$ContentPlaceHolder1$DonationControl1$chkAggregation"))
        isReportedDueToAggregation.map(_.isChecked().toString()).getOrElse("")
      },
      "isReportedUnder6212" -> asBoolean(record("Reported under 62:12")),
      "isSponsorship" -> asBoolean(record("Is sponsorship"))
    )
  }

}
