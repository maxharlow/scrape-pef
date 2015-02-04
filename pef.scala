import java.io.{File, StringReader}
import java.util.logging.{Logger, Level}
import java.util.concurrent.ForkJoinPool
import scala.concurrent.{Future, Await, blocking}
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.Duration
import scala.collection.immutable.ListMap
import scala.util.{Try, Success, Failure}
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

  implicit val context = ExecutionContext.fromExecutor(new ForkJoinPool(10))

  def run(filename: String) {
    val existing = CSVReader.open(filename).all.map(_.head)

    val append = Option(args(0) == "resume").getOrElse(false)
    val csv = CSVWriter.open(filename, append)
    if (!append) csv.writeRow(headers)

    def write(record: Map[String, String]): Unit = csv.writeRow(record.values.toSeq)

    val origin = CSVReader.open(new StringReader(source)).allWithHeaders.filterNot(existing contains _("EC reference")).view
    val process = lookup _ andThen write
    Await.result(Future.traverse(origin)(r => Future(process(r))), Duration.Inf)
  }

  def source: String = {
    println("Retrieving source...")
    val export = blocking {
      Logger.getLogger("com.gargoylesoftware").setLevel(Level.OFF)
      val client = new WebClient()
      client.getOptions.setThrowExceptionOnScriptError(false)
      val origin = client.getPage[HtmlPage]("https://pefonline.electoralcommission.org.uk/Search/CommonReturnsSearch.aspx")
      val search = origin.getElementByName[HtmlInput](controlSearch).click[HtmlPage]()
      val result = search.getElementByName[HtmlInput]("ctl00$ContentPlaceHolder1$searchControl1$btnGo").click[HtmlPage]()
      result.getElementByName[HtmlButton]("ctl00$ContentPlaceHolder1$searchControl1$btnExportAllResults").click[TextPage]()
    }
    export.getWebResponse().getContentAsString("ISO-8859-1")
  }

  def lookup(record: Map[String, String]): Map[String, String] = {
    val reference = record("EC reference")
    println(s"Looking up $reference")
    val page = retry(60) {
      blocking {
        Logger.getLogger("com.gargoylesoftware").setLevel(Level.OFF)
        val client = new WebClient()
        client.getOptions.setThrowExceptionOnScriptError(false)
        client.setAjaxController(new NicelyResynchronizingAjaxController())
        val origin = client.getPage[HtmlPage]("https://pefonline.electoralcommission.org.uk/Search/CommonReturnsSearch.aspx")
        val search = origin.getElementByName[HtmlInput](controlSearch).click[HtmlPage]()
        search.getElementByName[HtmlTextInput]("ctl00$ContentPlaceHolder1$searchControl1$txtECRefNo").`type`(reference)
        val result = search.getElementByName[HtmlInput]("ctl00$ContentPlaceHolder1$searchControl1$btnGo").click[HtmlPage]()
        client.waitForBackgroundJavaScriptStartingBefore(500)
        val detail = result.getElementById[HtmlAnchor](controlResult, false).click[HtmlPage]()
        client.waitForBackgroundJavaScriptStartingBefore(500)
        if (detail.getWebResponse().getContentAsString() contains reference) detail else throw new Exception("Unexpected page")
      }
    }
    select(record, page)
  }

  def select(record: Map[String, String], response: HtmlPage): Map[String, String]

  def stripTitles(name: String): String = {
    val prefixes = List("na", "Ms", "Mrs", "Miss", "Mr", "Dr", "Cllr", "Sir", "Dame", "Hon", "The Hon", "Rt Hon", "The Rt Hon")
    val suffixes = List("Deceased", "QC", "MP", "MSP", "AM", "MLA", "MEP", "OBE", "MBE", "CBE")
    val titlesRegex = (prefixes.map("^(" + _ + " )") ++ suffixes.map("( " + _ + ")(,?)")).mkString("|")
    name.replaceAll(titlesRegex, "").replaceAll(titlesRegex, "") // apply it twice for titles such as 'the rt hon sir'
  }

  def stripFakePostcodes(postcode: String): String = {
    postcode.replaceAll("ZZ0 0ZZ|ZZ00ZZ|ZZ1 1ZZ|ZZ11ZZ|AA0 0AA|AA00AA|AA1 1AA|AA11AA", "")
  }

  @annotation.tailrec
  final def retry[T](sleep: Int)(block: => T): T = {
    Try(block) match {
      case Success(x) => x
      case Failure(e) if sleep < 3600 => {
        println(s"Failed: ${e.getMessage}. Waiting ${sleep}s before retrying...")
        Thread.sleep(sleep * 1000) // seconds to milliseconds
        retry(sleep * 2)(block)
      }
      case Failure(e) => throw e
    }
  }

  def asDate(value: String, format: String): String = {
    val formatPattern = DateTimeFormat.forPattern(format)
    if (value.isEmpty) ""
    else DateTime.parse(value, formatPattern).toString("yyyy-MM-dd")
  }

  def asBoolean(value: String): String = {
    if (value.isEmpty) "false" else "true"
  }

}
