import scala.util.{Try, Success, Failure}
import org.joda.time.DateTime

object Start extends App {

  println("""
 ____  _       _             
|  _ \| |_   _| |_ _   _ ___ 
| |_) | | | | | __| | | / __|
|  __/| | |_| | |_| |_| \__ \
|_|   |_|\__,_|\__|\__,_|___/

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
