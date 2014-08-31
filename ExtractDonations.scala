import java.io.{File, StringReader}
import java.util.logging.{Logger, Level}
import scala.util.Try
import scala.collection.immutable.ListMap
import com.gargoylesoftware.htmlunit.{WebClient, TextPage}
import com.gargoylesoftware.htmlunit.html._
import com.github.tototoshi.csv.{CSVReader, CSVWriter}
import Common._

object ExtractDonations extends App {

  println("""
    ___  __     __
   / _ \/ /_ __/ /____
  / ___/ / // / __/ _ \
 /_/  /_/\_,_/\__/\___/

  """)

  run()

  def run() {
    val csv = CSVWriter.open(new File("donations.csv"))
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
      "ecReference",
      "type",
      "value",
      "acceptedDate",
      "receivedDate",
      "reportedDate",
      "nature",
      "purpose",
      "howDealtWith",
      "reportedUnder6212",
      "isSponsorship"
    )
    csv.writeRow(headers)
    for {
      year <- 2001 to 2014
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
      val searchPage = introPage.getElementByName[HtmlInput]("ctl00$ctl05$ctl01").click[HtmlPage]()
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
        val benefactorType = entry("Donor type")
        if (benefactorType == "Individual" || benefactorType == "Permitted Participant") "Individual"
        else if (benefactorType == "Registered Political Party") "Party"
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
      "benefactorCompanyNumber" -> entry("Company reg. no.").replaceAll("[^0+A-Za-z0-9]", "").replaceAll("^0*", ""), // optional
      "recipientClass" -> {
        val recipientType = entry("Entity type")
        val recipientRegulatedType = entry("Regulated donee type")
        if (recipientType == "Political Party" || recipientType == "Third Party") "Party"
        else if (recipientRegulatedType == "Members Association" || recipientRegulatedType == "Permitted Participant") "Organisation"
        else "Individual"
      },
      "recipientName" -> stripTitles(entry("Entity name")).replaceAll("Conservative and Unionist Party", "Conservative Party"),
      "recipientType" -> entry("Entity type"),
      "recipientRegulatedType" -> entry("Regulated donee type"), // optional
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
