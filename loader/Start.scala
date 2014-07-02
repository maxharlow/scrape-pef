import java.io.File

object Start extends App {

  println("""
   ___  __     __
  / _ \/ /_ __/ /____
 / ___/ / // / __/ _ \
/_/  /_/\_,_/\__/\___/

  """)

  val server = "localhost"
  val dataLocation = "../data"

  val files = new File(dataLocation).listFiles
  for (file <- files if file.getName matches "(donations-)\\d{4}.*(.csv)") {
    new Donations(server).run(file)
  }
  for (file <- files if file.getName matches "(loans-)\\d{4}.*(.csv)") {
    new Loans(server).run(file)
  }

}
