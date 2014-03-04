object Start extends App {

  println("Plutolatry!")

  Donations.run("data/donations-2013.csv")
  Loans.run("data/loans-2013.csv")
  Companies.run()

}
