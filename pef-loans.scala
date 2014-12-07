import scala.util.Try
import scala.collection.immutable.ListMap
import com.gargoylesoftware.htmlunit.html._

object Loans extends PEF {

  run("pef-loans.csv")

  val headers = List(
    "benefactorClass",
    "benefactorName",
    "benefactorType",
    "benefactorAddress",
    "benefactorPostcode",
    "benefactorCompanyNumber",
    "recipientClass",
    "recipientName",
    "recipientDeregisteredDate",
    "recipientType",
    "ecReference",
    "type",
    "value",
    "referenceNumber",
    "rate",
    "status",
    "amountRepaid",
    "amountConverted",
    "amountOutstanding",
    "startDate",
    "endDate",
    "repaidDate",
    "ecLastNotifiedDate",
    "recordedBy"
  )

  val controlSearch = "ctl00$ctl05$ctl07"
  val controlResult = "ctl00_ContentPlaceHolder1_searchControl1_grdLoanFullResults_ctl00_ctl04_lbViewLoanReturnItem"

  override def lookupList(response: HtmlPage): Map[String, String] = {
    ListMap(
      "organisation" -> response.getElementByName[HtmlSelect]("ctl00_ContentPlaceHolder1_LoanTransactionControl1_loanTransactionControl1_ddlOrganisation").getTextContent(),
      "reference" -> response.getElementByName[HtmlInput]("ctl00$ContentPlaceHolder1$LoanTransactionControl1$loanTransactionControl1$txtReference").getTextContent(),
      "enteredIntoDate" -> {
        val year = response.getElementByName[HtmlSelect]("ctl00$ContentPlaceHolder1$LoanTransactionControl1$loanTransactionControl1$dsDateEnteredInto$ddlYear").getTextContent()
        val month = response.getElementByName[HtmlSelect]("ctl00$ContentPlaceHolder1$LoanTransactionControl1$loanTransactionControl1$dsDateEnteredInto$ddlMonth").getTextContent()
        val day = response.getElementByName[HtmlSelect]("ctl00$ContentPlaceHolder1$LoanTransactionControl1$loanTransactionControl1$dsDateEnteredInto$ddlDay").getTextContent()
        s"$year-$month-$day"
      },
      "repaymentTerm" -> response.getElementByName[HtmlSelect]("ctl00$ContentPlaceHolder1$LoanTransactionControl1$loanTransactionControl1$ddlLoanRepaymentTerm").getTextContent(),
      "repayableDate" -> {
        val year = response.getElementByName[HtmlSelect]("ctl00$ContentPlaceHolder1$LoanTransactionControl1$loanTransactionControl1$dsDateRepayable$ddlYear").getTextContent()
        val month = response.getElementByName[HtmlSelect]("ctl00$ContentPlaceHolder1$LoanTransactionControl1$loanTransactionControl1$dsDateRepayable$ddlMonth").getTextContent()
        val day = response.getElementByName[HtmlSelect]("ctl00$ContentPlaceHolder1$LoanTransactionControl1$loanTransactionControl1$dsDateRepayable$ddlDay").getTextContent()
        val date = s"$year-$month-$day"
        if (date != "----------") date else ""
      },
      "value" -> response.getElementByName[HtmlInput]("ctl00$ContentPlaceHolder1$LoanTransactionControl1$loanTransactionControl1$txtLoanValue").getTextContent(),
      "interestRate" -> response.getElementByName[HtmlInput]("ctl00$ContentPlaceHolder1$LoanTransactionControl1$loanTransactionControl1$txtRateOfInterest").getTextContent(),
      "interestRateFixed" -> response.getElementByName[HtmlCheckBoxInput]("ctl00_ContentPlaceHolder1_LoanTransactionControl1_loanTransactionControl1_chkRateOfInterestFixed").isChecked().toString(),
      "interestRateVariable" -> response.getElementByName[HtmlCheckBoxInput]("ctl00_ContentPlaceHolder1_LoanTransactionControl1_loanTransactionControl1_chkRateOfInterestVariable").isChecked().toString(),
      "additionalInformation" -> response.getElementByName[HtmlTextArea]("ctl00$ContentPlaceHolder1$LoanTransactionControl1$loanTransactionControl1$txtAdditionalInformation").getTextContent(),
      "benefactorStatus" -> response.getElementByName[HtmlSelect]("ctl00$ContentPlaceHolder1$LoanTransactionControl1$loanTransactionControl1$participant1$ddlParticipantStatus").getTextContent(),
      "benefactorTitle" -> {
        val title = response.getElementByName[HtmlSelect]("ctl00$ContentPlaceHolder1$LoanTransactionControl1$loanTransactionControl1$participant1$ddlTitle").getTextContent()
        if (title contains "Please Select") "" else title
      },
      "benefactorFirstName" -> {
        val firstName = response.getElementByName[HtmlInput]("ctl00$ContentPlaceHolder1$LoanTransactionControl1$loanTransactionControl1$participant1$txtFirstname").getTextContent()
        if (firstName == "na") "" else firstName
      },
      "benefactorMiddleName" -> response.getElementByName[HtmlInput]("ctl00$ContentPlaceHolder1$LoanTransactionControl1$loanTransactionControl1$participant1$txtMiddlename").getTextContent(),
      "benefactorLastName" -> response.getElementByName[HtmlInput]("ctl00$ContentPlaceHolder1$LoanTransactionControl1$loanTransactionControl1$participant1$txtSurname").getTextContent(),
      "benefactorCompanyName" -> {
        response.getElementByName[HtmlInput]("ctl00$ContentPlaceHolder1$LoanTransactionControl1$loanTransactionControl1$participant1$txtParticupantName").getTextContent()
      }
    )
  }

  override def select(entry: Map[String, String]): Map[String, String] = {
    ListMap(
      "benefactorClass" -> {
        val benefactorType = entry("Lender type")
        if (benefactorType == "Individual") "Individual"
        else if (benefactorType == "Registered Political Party") "Party"
        else "Organisation"
      },
      "benefactorName" -> {
        val name = entry("Lender name").replaceAll(", ([0-9]),", ", $1")
        if (entry("Lender type") == "Individual") stripTitles(name)
        else if (name contains ", ") name.split(", ").init.mkString(", ") // split from address
        else name
      },
      "benefactorType" -> entry("Lender type"),
      "benefactorAddress" -> {
        val name = entry("Lender name").replaceAll(", ([0-9]),", ", $1")
        if (entry("Lender type") == "Individual") ""
        else if (name contains ", ") name.split(", ").last.replaceAll("^(A)$|^(NA)$", "") // split from name
        else ""
      },
      "benefactorPostcode" -> stripFakePostcodes(entry("Postcode")), // optional
      "benefactorCompanyNumber" -> {
        val benefactorType = entry("Lender type")
        if (benefactorType == "Registered Political Party") "" // parties shouldn't have company numbers
        else entry("Company reg. no.").replaceAll("[^0+A-Za-z0-9]", "").replaceAll("^0*", "") // optional
      },
      "recipientClass" -> {
        val recipientType = entry("Entity type")
        if (recipientType == "Political Party" || recipientType == "Third Party") "Party"
        else "Organisation" // cannot loan to individuals
      },
      "recipientName" -> stripTitles(entry("Entity name")).replaceAll(""" \[De-registered .*\]""", "").replaceAll("Conservative and Unionist Party", "Conservative Party"),
      "recipientDeregisteredDate" -> { // optional, de-registered parties only
        val recipientName = entry("Entity name")
        if (recipientName contains "De-registered") asDate(recipientName.replaceAll(".*De-registered ", ""), "dd/MM/yy]")
        else ""
      },
      "recipientType" -> entry("Entity type"),
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
      "recordedBy" -> entry("Rec'd by (AU)") // optional
    )
  }

}
