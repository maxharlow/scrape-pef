Party funding
=============

Political parties are mostly funded by membership subscriptions, state funding, and donations/loans. This mostly looks at the latter.


Data
----

### Donations and loans

Donations and loans over £500 must be declared to the [Electoral Commission] (http://www.electoralcommission.org.uk/). This data is available from their [parties and elections finance (PEF) database] (https://pefonline.electoralcommission.org.uk/search/searchintro.aspx), which allows you to export CSVs. Data appears to be available from the database about a month after being reported.

Note that the PEF database does not include payments to parties in Northern Ireland until the [Northern Ireland (Miscellaneous Provisions) Bill] (http://www.publications.parliament.uk/pa/bills/cbill/2013-2014/0009/2014009.pdf) is passed.


Grapher
-------

Iterate through a donations CSV produced by PEF and create a [Neo4j] (http://www.neo4j.org/) graph database. Requires [SBT] (http://www.scala-sbt.org/) to be installed and Neo4j running.

Note the format of PEF CSVs is pretty idiosyncratic: commas at the end of lines, Windows line-endings, and the last four characters of each file appears to be missing. You will only need to manually fix the latter for the Grapher to understand it.

Usage:

    sbt "run ../data/donations-2013.csv"
