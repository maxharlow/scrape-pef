import scala.util.{Try, Success, Failure}
import org.joda.time.DateTime

object Start extends App {

  println("""
   ___  __     __
  / _ \/ /_ __/ /____
 / ___/ / // / __/ _ \
/_/  /_/\_,_/\__/\___/

  """)

  println("Enter a start date:")
  val periodStartDate = readDate
  println("Enter an end date:")
  val periodEndDate = readDate

  if (periodEndDate isBefore periodStartDate) sys.error("End is before the start")

  Donations.run(periodStartDate, periodEndDate)
  Loans.run(periodStartDate, periodEndDate)
  Companies.run(periodStartDate, periodEndDate)
  Members.run(periodStartDate, periodEndDate)

  def readDate: DateTime = {
    val dateString = readLine("-> ")
    Try(new DateTime(dateString)) match {
      case Success(date) => {
        println("Using " + date.toString("yyyy-MM-dd") + "...\n")
        date
      }
      case Failure(e) => {
        println("Invalid date, try again.")
        readDate
      }
    }
  }

}
