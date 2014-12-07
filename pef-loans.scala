import scala.collection.immutable.ListMap

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
