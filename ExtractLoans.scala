import java.io.{File, StringReader}
import java.util.logging.{Logger, Level}
import scala.util.Try
import scala.collection.immutable.ListMap
import com.gargoylesoftware.htmlunit.{WebClient, TextPage}
import com.gargoylesoftware.htmlunit.html._
import com.github.tototoshi.csv.{CSVReader, CSVWriter}
import Common._

object ExtractLoans extends App {

  println("""
    ___  __     __
   / _ \/ /_ __/ /____
  / ___/ / // / __/ _ \
 /_/  /_/\_,_/\__/\___/

  """)

  run()

  def run() {
    val csv = CSVWriter.open(new File("loans.csv"))
    val headers = List(
      "benefactorClass",
      "benefactorName",
      "benefactorType",
      "benefactorAddress",
      "benefactorPostcode",
      "benefactorCompanyNumber",
      "recipientClass",
      "recipientName",
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
    csv.writeRow(headers)

    for {
      year <- 1987 to 2014
    }
    yield for (data <- retrieve(year))
    yield for (entry <- data.allWithHeaders) {
      val selected = select(entry mapValues clean)
      csv.writeRow(selected.values.toSeq)
    }
    csv.close()
  }

  def retrieve(year: Int): Try[CSVReader] = {
    println(s"Now retrieving $year...")
    val response = Try {
      Logger.getLogger("com.gargoylesoftware").setLevel(Level.OFF)
      val client = new WebClient
      client.getOptions.setThrowExceptionOnScriptError(false)
      val introPage = client.getPage[HtmlPage]("https://pefonline.electoralcommission.org.uk/Search/CommonReturnsSearch.aspx")
      val searchPage = introPage.getElementByName[HtmlInput]("ctl00$ctl05$ctl07").click[HtmlPage]()
      searchPage.getElementByName[HtmlSelect]("ctl00$ContentPlaceHolder1$searchControl1$dtAcceptedFrom$ddlDay").setSelectedAttribute("1", true)
      searchPage.getElementByName[HtmlSelect]("ctl00$ContentPlaceHolder1$searchControl1$dtAcceptedFrom$ddlMonth").setSelectedAttribute("1", true)
      searchPage.getElementByName[HtmlSelect]("ctl00$ContentPlaceHolder1$searchControl1$dtAcceptedFrom$ddlYear").setSelectedAttribute(year.toString, true)
      searchPage.getElementByName[HtmlSelect]("ctl00$ContentPlaceHolder1$searchControl1$dtAcceptedTo$ddlDay").setSelectedAttribute("31", true)
      searchPage.getElementByName[HtmlSelect]("ctl00$ContentPlaceHolder1$searchControl1$dtAcceptedTo$ddlMonth").setSelectedAttribute("12", true)
      searchPage.getElementByName[HtmlSelect]("ctl00$ContentPlaceHolder1$searchControl1$dtAcceptedTo$ddlYear").setSelectedAttribute(year.toString, true)
      val resultsPage = searchPage.getElementByName[HtmlInput]("ctl00$ContentPlaceHolder1$searchControl1$btnGo").click[HtmlPage]()
      resultsPage.getElementByName[HtmlButton]("ctl00$ContentPlaceHolder1$searchControl1$btnExportAllResults").click[TextPage]().getWebResponse().getContentAsString("ISO-8859-1")
    }
    response recover {
      case e => println(s"FAILED TO LOAD YEAR $year: ${e.getMessage}")
    }
    response map { r =>
      CSVReader.open(new StringReader(r))
    }
  }

  def select(entry: Map[String, String]): Map[String, String] = {
    ListMap(
      "benefactorClass" -> {
        val benefactorType = entry("Lender type")
        if (benefactorType == "Individual") "Individual"
        else if (benefactorType == "Registered Political Party") "Party"
        else "Organisation"
      },
      "benefactorName" -> {
        val name = entry("Lender name")
        if (entry("Lender type") == "Individual") stripTitles(name)
        else if (!name.contains(", ")) name
        else name.split(", ").head // split from address
      },
      "benefactorType" -> entry("Lender type"),
      "benefactorAddress" -> {
        val name = entry("Lender name")
        if (entry("Lender type") == "Individual") ""
        else if (!name.contains(", ")) ""
        else name.split(", ").tail.mkString(", ").replaceAll("^(A)$|^(NA)$", "") // split from name
      },
      "benefactorPostcode" -> stripFakePostcodes(entry("Postcode")), // optional
      "benefactorCompanyNumber" -> entry("Company reg. no.").replaceAll("[^0+A-Za-z0-9]", "").replaceAll("^0*", ""), // optional
      "recipientClass" -> {
        val recipientType = entry("Entity type")
        if (recipientType == "Political Party" || recipientType == "Third Party") "Party"
        else "Organisation" // cannot loan to individuals
      },
      "recipientName" -> stripTitles(entry("Entity name")).replaceAll("Conservative and Unionist Party", "Conservative Party"),
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
