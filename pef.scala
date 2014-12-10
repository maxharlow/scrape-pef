import java.io.{File, StringReader}
import java.util.logging.{Logger, Level}
import scala.concurrent.{Future, Await}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.collection.immutable.ListMap
import scala.util.Try
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import org.json4s.{JValue, JString, JBool, JNull}
import com.gargoylesoftware.htmlunit.{WebClient, TextPage, NicelyResynchronizingAjaxController}
import com.gargoylesoftware.htmlunit.html._
import com.github.tototoshi.csv.{CSVReader, CSVWriter}

trait PEF extends App {

  val headers: List[String]
  val controlSearch: String
  val controlResult: String

  def run(filename: String) {
    val csv = CSVWriter.open(filename)
    csv.writeRow(headers)

    def write(record: Map[String, String]): Unit = csv.writeRow(record.values.toSeq)

    val origin = CSVReader.open(new StringReader(source)).allWithHeaders.view
    val process = lookup _ andThen write
    Await.result(Future.traverse(origin)(r => Future(process(r))), Duration.Inf)

    csv.close()
  }

  def source: String = {
    println("Retrieving source...")
    Logger.getLogger("com.gargoylesoftware").setLevel(Level.OFF)
    val client = new WebClient()
    client.getOptions.setThrowExceptionOnScriptError(false)
    val origin = client.getPage[HtmlPage]("https://pefonline.electoralcommission.org.uk/Search/CommonReturnsSearch.aspx")
    val search = origin.getElementByName[HtmlInput](controlSearch).click[HtmlPage]()
    val result = search.getElementByName[HtmlInput]("ctl00$ContentPlaceHolder1$searchControl1$btnGo").click[HtmlPage]()
    val export = result.getElementByName[HtmlButton]("ctl00$ContentPlaceHolder1$searchControl1$btnExportAllResults").click[TextPage]()
    export.getWebResponse().getContentAsString("ISO-8859-1")
  }

  def lookup(record: Map[String, String]): Map[String, String] = {
    val reference = record("EC reference")
    println(s"Looking up $reference")
    val page = {
      Logger.getLogger("com.gargoylesoftware").setLevel(Level.OFF)
      val client = new WebClient()
      client.getOptions.setThrowExceptionOnScriptError(false)
      client.setAjaxController(new NicelyResynchronizingAjaxController())
      val origin = client.getPage[HtmlPage]("https://pefonline.electoralcommission.org.uk/Search/CommonReturnsSearch.aspx")
      val search = origin.getElementByName[HtmlInput](controlSearch).click[HtmlPage]()
      search.getElementByName[HtmlTextInput]("ctl00$ContentPlaceHolder1$searchControl1$txtECRefNo").`type`(reference)
      val result = search.getElementByName[HtmlInput]("ctl00$ContentPlaceHolder1$searchControl1$btnGo").click[HtmlPage]()
      client.waitForBackgroundJavaScriptStartingBefore(500)
      result.getElementById[HtmlAnchor](controlResult, false).click[HtmlPage]()
    }
    select(record, page)
  }

  def select(record: Map[String, String], response: HtmlPage): Map[String, String]

  def stripTitles(name: String): String = {
    val cleanName = name.replaceAll("^(na )|( na)", "")
    val prefixes = List("Ms", "Mrs", "Miss", "Mr", "Dr", "Cllr", "Sir", "Dame", "Hon", "The Hon", "Rt Hon", "The Rt Hon")
    val suffixes = List("Deceased", "QC", "MP", "MSP", "AM", "MLA", "MEP", "OBE", "MBE", "CBE")
    val titlesRegex = (prefixes.map("^(" + _ + " )") ++ suffixes.map("( " + _ + ")(,?)")).mkString("|")
    cleanName.replaceAll(titlesRegex, "").replaceAll(titlesRegex, "") // apply it twice for titles such as 'the rt hon sir'
  }

  def stripFakePostcodes(postcode: String): String = {
    postcode.replaceAll("ZZ0 0ZZ|ZZ00ZZ|ZZ1 1ZZ|ZZ11ZZ|AA0 0AA|AA00AA|AA1 1AA|AA11AA", "")
  }

  def asDate(value: String, format: String): String = {
    val formatPattern = DateTimeFormat.forPattern(format)
    if (value.isEmpty) ""
    else DateTime.parse(value, formatPattern).toString("yyyy-MM-dd")
  }

  def asInt(value: String): String = {
    value.replaceAll("[^0-9]", "")
  }

  def asBoolean(value: String): String = {
    if (value.isEmpty) "false" else "true"
  }

}
