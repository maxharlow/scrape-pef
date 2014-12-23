Scrape PEF
==========

The [Electoral Commission] (http://www.electoralcommission.org.uk/) run the [Party and Electoral Finance (PEF)] (https://pefonline.electoralcommission.org.uk/Search/SearchIntro.aspx) site, which lists political donations and loans. This scrapes both those sets of records into CSVs.

Requires [SBT] (http://www.scala-sbt.org/) and the JDK.

Run with `sbt run`. It will prompt to scrape either donations or loans.


Data
----

Parties must record all donations and loans over £500. If a given benefactor has donated over £7,500 in either one or multiple donations/loans to the party which total that amount, they must be declared to the [Electoral Commission] (http://www.electoralcommission.org.uk/). If the party has already reported donations or loans from that benefactor that year, the threshold for declaration is lower: £1,500. This data is published on their [parties and elections finance (PEF) database] (https://pefonline.electoralcommission.org.uk/search/searchintro.aspx). Data appears to be available from the database within a fortnight of being reported. Parties must report to the comission quarterly:

 * Q1: 30th April
 * Q2: 30th July
 * Q3: 30th October
 * Q4: 30th January

After a general election has been called things are different: reports must be made weekly, unless the party has declared that they will be standing no candidates.

Note that the PEF database does not include payments to parties in Northern Ireland until the the [Northern Ireland (Miscellaneous Provisions) Bill] (http://www.publications.parliament.uk/pa/bills/cbill/2013-2014/0009/2014009.pdf) was passed.

