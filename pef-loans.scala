import scala.util.Try
import scala.collection.immutable.ListMap
import scala.collection.JavaConversions._
import com.gargoylesoftware.htmlunit.html._

object Loans extends PEF {

  override val headers = List(
    "ecReference",
    "ecLastNotifiedDate",
    "ecPublishedDate",
    "ecReleaseTitle",
    "lenderType",
    "lenderTitle",
    "lenderFirstName",
    "lenderMiddleName",
    "lenderLastName",
    "lenderName",
    "lenderCompanyNumber",
    "lenderAddress",
    "lenderPostcode",
    "lenderCountry",
    "lenderPhoneNumber",
    "lenderEmailAddress",
    "recipientName",
    "recipientID",
    "recipientType",
    "recipientRegulatedType",
    "recipientDeregisteredDate",
    "accountingUnit",
    "accountingUnitID",
    "value",
    "valueRepaid",
    "valueConverted",
    "valueOutstanding",
    "type",
    "rate",
    "rateFixed",
    "rateVariable",
    "status",
    "repaymentTerm",
    "startDate",
    "endDate",
    "repaidDate",
    "referenceNumber",
    "notes",
    "additionalInformation",
    "isReportedDueToAggregation",
    "hasSecurityBeenGiven"
  )

  override val controlSearch = "ctl00$ctl05$ctl07"
  override val controlResult = "ctl00_ContentPlaceHolder1_searchControl1_grdLoanFullResults_ctl00_ctl04_lbViewLoanReturnItem"

  run("pef-loans.csv")

  override def select(record: Map[String, String], page: HtmlPage): Map[String, String] = {
    ListMap(
      "ecReference" -> record("EC reference"),
      "ecLastNotifiedDate" -> asDate(record("Date EC last notified"), "dd/MM/yyyy"),
      "ecPublishedDate" -> {
        val publishedDate = Try(page.getElementById[HtmlSpan]("ctl00_ContentPlaceHolder1_LoanTransactionControl1_lblPublishedDateValue", true))
        publishedDate.map(_.getTextContent()).map(asDate(_, "dd/MM/yyyy")).getOrElse("")
      },
      "ecReleaseTitle" -> page.getElementById[HtmlSpan]("ctl00_ContentPlaceHolder1_LoanTransactionControl1_lblTransactionTitle", true).getTextContent(),
      "lenderType" -> record("Lender type"),
      "lenderTitle" -> { // individuals only
        val lenderTitle = Try(page.getElementByName[HtmlSelect]("ctl00$ContentPlaceHolder1$LoanTransactionControl1$loanTransactionControl1$participant1$ddlTitle"))
        val lenderTitleText = lenderTitle.map(_.getSelectedOptions().head.getTextContent()).getOrElse("")
        if (lenderTitleText matches "na|-- Please Select --") "" else lenderTitleText
      },
      "lenderFirstName" -> { // individuals only
        val lenderFirstName = Try(page.getElementByName[HtmlInput]("ctl00$ContentPlaceHolder1$LoanTransactionControl1$loanTransactionControl1$participant1$txtFirstname"))
        val lenderFirstNameText = lenderFirstName.map(_.getValueAttribute()).getOrElse("")
        if (lenderFirstNameText == "na") "" else lenderFirstNameText
      },
      "lenderMiddleName" -> { // individuals only
        val lenderMiddleName = Try(page.getElementByName[HtmlInput]("ctl00$ContentPlaceHolder1$LoanTransactionControl1$loanTransactionControl1$participant1$txtMiddlename"))
        val lenderMiddleNameText = lenderMiddleName.map(_.getValueAttribute()).getOrElse("")
        if (lenderMiddleNameText == "na") "" else lenderMiddleNameText
      },
      "lenderLastName" -> { // individuals only
        val lenderLastName = Try(page.getElementByName[HtmlInput]("ctl00$ContentPlaceHolder1$LoanTransactionControl1$loanTransactionControl1$participant1$txtSurname"))
        val lenderLastNameText = lenderLastName.map(_.getValueAttribute()).getOrElse("")
        if (lenderLastNameText == "na") "" else lenderLastNameText
      },
      "lenderName" -> { // non-individuals only
        val lenderName = Try(page.getElementByName[HtmlInput]("ctl00$ContentPlaceHolder1$LoanTransactionControl1$loanTransactionControl1$participant1$txtParticupantName"))
        val lenderNameText = lenderName.map(_.getValueAttribute()).getOrElse("")
        if (lenderNameText == "na") "" else lenderNameText
      },
      "lenderCompanyNumber" -> {
        val lenderCompanyNumber = Try(page.getElementByName[HtmlInput]("ctl00$ContentPlaceHolder1$LoanTransactionControl1$loanTransactionControl1$participant1$txtCompanyRegistrationNumber"))
        lenderCompanyNumber.map(_.getValueAttribute()).getOrElse("")
      },
      "lenderAddress" -> {
        val lenderAddress = List(
          Try(page.getElementByName[HtmlInput]("ctl00$ContentPlaceHolder1$LoanTransactionControl1$loanTransactionControl1$participant1$txtAddressLine1")).map(_.getValueAttribute()),
          Try(page.getElementByName[HtmlInput]("ctl00$ContentPlaceHolder1$LoanTransactionControl1$loanTransactionControl1$participant1$txtAddressLine2")).map(_.getValueAttribute()),
          Try(page.getElementByName[HtmlInput]("ctl00$ContentPlaceHolder1$LoanTransactionControl1$loanTransactionControl1$participant1$txtAddressLine3")).map(_.getValueAttribute()),
          Try(page.getElementByName[HtmlInput]("ctl00$ContentPlaceHolder1$LoanTransactionControl1$loanTransactionControl1$participant1$txtAddressLine4")).map(_.getValueAttribute()),
          Try(page.getElementByName[HtmlInput]("ctl00$ContentPlaceHolder1$LoanTransactionControl1$loanTransactionControl1$participant1$txtTown")).map(_.getValueAttribute()).map(_.replace("no town", "")),
          Try(page.getElementByName[HtmlSelect]("ctl00$ContentPlaceHolder1$LoanTransactionControl1$loanTransactionControl1$participant1$ddlCounty")).map(_.getSelectedOptions().head.getTextContent())
        )
        lenderAddress.map(_.getOrElse("")).filterNot(_.isEmpty).mkString(", ")
      },
      "lenderPostcode" -> {
        val lenderPostcode = Try(page.getElementByName[HtmlInput]("ctl00$ContentPlaceHolder1$LoanTransactionControl1$loanTransactionControl1$participant1$txtPostcode"))
        lenderPostcode.map(_.getValueAttribute()).map(stripFakePostcodes).getOrElse("")
      },
      "lenderCountry" -> {
        val lenderCountry = Try(page.getElementByName[HtmlSelect]("ctl00$ContentPlaceHolder1$LoanTransactionControl1$loanTransactionControl1$participant1$ddlCountry"))
        val lenderCountryText = lenderCountry.map(_.getSelectedOptions().head.getTextContent()).getOrElse("")
        if (lenderCountryText contains "Please Select") "" else lenderCountryText
      },
      "lenderPhoneNumber" -> {
        val lenderPhoneNumber = Try(page.getElementByName[HtmlInput]("ctl00$ContentPlaceHolder1$LoanTransactionControl1$loanTransactionControl1$participant1$txtPhoneNumber"))
        lenderPhoneNumber.map(_.getValueAttribute()).getOrElse("")
      },
      "lenderEmailAddress" -> {
        val lenderEmailAddress = Try(page.getElementByName[HtmlInput]("ctl00$ContentPlaceHolder1$LoanTransactionControl1$loanTransactionControl1$participant1$txtEmail"))
        lenderEmailAddress.map(_.getValueAttribute()).getOrElse("")
      },
      "recipientName" -> {
        val recipientName = stripTitles(record("Entity name")).replaceAll(""" \[De-registered .*\]""", "")
        recipientName.replaceAll("Conservative and Unionist Party", "Conservative Party")
      },
      "recipientID" -> record("Entity ID"),
      "recipientType" -> record("Entity type"),
      "recipientRegulatedType" -> {
        val recipientRegulatedType = Try(page.getElementByName[HtmlSelect]("ctl00$ContentPlaceHolder1$LoanTransactionControl1$loanTransactionControl1$ddlRegulatedDoneeStatus"))
        recipientRegulatedType.map(_.getSelectedOptions().head.getTextContent()).getOrElse("")
      },
      "recipientDeregisteredDate" -> { // de-registered parties only
        val recipientName = record("Entity name")
        if (recipientName contains "De-registered") asDate(recipientName.replaceAll(".*De-registered ", ""), "dd/MM/yy]")
        else ""
      },
      "accountingUnit" -> { // aka 'organisation entering into the transaction'
        val accountingUnit = Try(page.getElementByName[HtmlSelect]("ctl00$ContentPlaceHolder1$LoanTransactionControl1$loanTransactionControl1$ddlOrganisation"))
        accountingUnit.map(_.getSelectedOptions().head.getTextContent()).getOrElse("")
      },
      "accountingUnitID" -> record("Accounting unit ID"),
      "value" -> record("Total amount"),
      "valueRepaid" -> record("Amount repaid"),
      "valueConverted" -> record("Amount converted"),
      "valueOutstanding" -> record("Amount outstanding"),
      "type" -> record("Type of borrowing"),
      "rate" -> record("Rate"),
      "rateFixed" -> {
        val rateFixed = Try(page.getElementByName[HtmlCheckBoxInput]("ctl00$ContentPlaceHolder1$LoanTransactionControl1$loanTransactionControl1$chkRateOfInterestFixed"))
        rateFixed.map(_.isChecked().toString()).getOrElse("")
      },
      "rateVariable" -> {
        val rateVariable = Try(page.getElementByName[HtmlCheckBoxInput]("ctl00$ContentPlaceHolder1$LoanTransactionControl1$loanTransactionControl1$chkRateOfInterestVariable"))
        rateVariable.map(_.isChecked().toString()).getOrElse("")
      },
      "status" -> record("Status"),
      "repaymentTerm" -> {
        val repaymentTerm = Try(page.getElementByName[HtmlSelect]("ctl00$ContentPlaceHolder1$LoanTransactionControl1$loanTransactionControl1$ddlLoanRepaymentTerm"))
        repaymentTerm.map(_.getSelectedOptions().head.getTextContent()).getOrElse("")
      },
      "startDate" -> asDate(record("Start date"), "dd/MM/yyyy"), // aka 'entered into date'
      "endDate" -> asDate(record("End date"), "dd/MM/yyyy"), // aka 'repayable date', 'end/review date'
      "repaidDate" -> asDate(record("Date repaid"), "dd/MM/yyyy"),
      "referenceNumber" -> record("Loan reference no."),
      "notes" -> {
        val notes = Try(page.getElementByName[HtmlInput]("ctl00$ContentPlaceHolder1$LoanTransactionControl1$loanTransactionControl1$txtNatureOfTransactionNotes"))
        notes.map(_.getValueAttribute()).getOrElse("")
      },
      "additionalInformation" -> {
        val additionalInformation = Try(page.getElementByName[HtmlTextArea]("ctl00$ContentPlaceHolder1$LoanTransactionControl1$loanTransactionControl1$txtAdditionalInformation"))
        additionalInformation.map(_.getTextContent()).getOrElse("")
      },
      "isReportedDueToAggregation" -> {
        val isReportedDueToAggregation = Try(page.getElementByName[HtmlCheckBoxInput]("ctl00$ContentPlaceHolder1$LoanTransactionControl1$loanTransactionControl1$chkReportedBecauseOfAggregation"))
        isReportedDueToAggregation.map(_.isChecked().toString()).getOrElse("")
      },
      "hasSecurityBeenGiven" -> {
        val hasSecurityBeenGiven = Try(page.getElementByName[HtmlCheckBoxInput]("ctl00$ContentPlaceHolder1$LoanTransactionControl1$loanTransactionControl1$chkHasSecurityBeenGiven"))
        hasSecurityBeenGiven.map(_.isChecked().toString()).getOrElse("")
      }
    )
  }

}
