Plutolatry
==========

Who funds political parties, and why?


Data
----

### Donations and loans

Parties must record all donations and loans over £500. If a given benefactor has donated over £7,500 in either one or multiple donations/loans to the party which total that amount, they must be declared to the [Electoral Commission] (http://www.electoralcommission.org.uk/). If the party has already reported donations or loans from that benefactor that year, the threshold for declaration is lower: £1,500. This data is published on their [parties and elections finance (PEF) database] (https://pefonline.electoralcommission.org.uk/search/searchintro.aspx), from which CSVs can be exported. Data appears to be available from the database about a month after being reported. Parties must report to the comission quarterly:

 * Q1: 30th April
 * Q2: 30th July
 * Q3: 30th October
 * Q4: 30th January

After a general election has been called things are different: reports must be made weekly, unless the party has declared that they will be standing no candidates.

Note that the PEF database does not include payments to parties in Northern Ireland until the [Northern Ireland (Miscellaneous Provisions) Bill] (http://www.publications.parliament.uk/pa/bills/cbill/2013-2014/0009/2014009.pdf) is passed.

Note the format of PEF CSVs is pretty idiosyncratic: commas at the end of lines, Windows line-endings, and the last few characters at the end of each file are often missing. You will only need to manually fix the latter for this application to understand it.

This iterates through loan or donation CSVs produced by PEF and creates benefactor nodes, recipient nodes, and relationships between the two for each transaction.

### Companies

PEF data includes company registration numbers. [Companies House] (http://www.companieshouse.gov.uk/) holds detailed company information against these numbers, including directorships and other officer positions. Sadly they have no reasonable way of accessing that data, but [OpenCorporates] (https://opencorporates.com/) scrape company data from there and many other sources.

This uses the OpenCorporates API to improve the information attached to benefactor companies and create officership relationships between companies and people already in the graph.


Running
-------

Requires [SBT] (http://www.scala-sbt.org/) and the [Neo4j] (http://www.neo4j.org/) graph database to be installed and running. There also needs to be a configuration file, `plutolatry.conf`, which might look like:

    openCorporatesKey = "abcdefghijklm"
    donationsData = "data/donations-2013.csv"
    loansData = "data/loans-2013.csv"

Usage:

    sbt run

It takes a while. The database produced can be seen in the Neo4j browser: http://localhost:7474/
