import java.io.File

object Start extends App {

  println("""
    ___  __     __
   / _ \/ /_ __/ /____
  / ___/ / // / __/ _ \
 /_/  /_/\_,_/\__/\___/

  """)

  println("Loading CSVs...")

  val dataLocation = "../data"

  // main data sources (donations and loans)
  val files = new File(dataLocation).listFiles
  for (file <- files if file.getName matches "(donations-)\\d{4}.*(-clean.csv)") {
    new Donations(file).loadFile()
  }
  for (file <- files if file.getName matches "(loans-)\\d{4}.*(-clean.csv)") { // todo -- companies
    new Loans(file).loadFile()
  }

  // and now some context
  Companies.run()
  Members.run()

}
