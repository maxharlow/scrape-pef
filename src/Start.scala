import scala.util.{Try, Success, Failure}
import org.joda.time.DateTime

object Start extends App {

  println("""
 ______   __       __  __   _________  ______   __       ________   _________  ______    __  __   
/_____/\ /_/\     /_/\/_/\ /________/\/_____/\ /_/\     /_______/\ /________/\/_____/\  /_/\/_/\  
\   _ \ \\ \ \    \ \ \ \ \\__    __\/\   _ \ \\ \ \    \    _  \ \\__    __\/\   _ \ \ \ \ \ \ \ 
 \ (_) \ \\ \ \    \ \ \ \ \  \  \ \   \ \ \ \ \\ \ \    \  (_)  \ \  \  \ \   \ (_) ) )_\ \_\ \ \
  \  ___\/ \ \ \____\ \ \ \ \  \  \ \   \ \ \ \ \\ \ \____\   __  \ \  \  \ \   \  __ `\ \\    _\/
   \ \ \    \ \/___/\\ \_\ \ \  \  \ \   \ \_\ \ \\ \/___/\\  \ \  \ \  \  \ \   \ \ `\ \ \ \  \ \
    \_\/     \_____\/ \_____\/   \__\/    \_____\/ \_____\/ \__\/\__\/   \__\/    \_\/ \_\/  \__\/
  """)

  println("Enter a start date:")
  val periodStartDate = readDate

  println("Enter an end date:")
  val periodEndDate = readDate

  Donations.run(Config.donationsData)
  Loans.run(Config.loansData)

  Companies.run(periodStartDate, periodEndDate)
  Members.run(periodStartDate, periodEndDate)

  def readDate: DateTime = {
    val dateString = readLine("-> ")
    Try(new DateTime(dateString)) match {
      case Success(date) => {
        println("Using " + date.toString("yyyy-MM-dd") + "...")
        date
      }
      case Failure(e) => {
        println("Invalid date, try again.")
        readDate
      }
    }
  }

}
