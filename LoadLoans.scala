import java.io.File
import org.anormcypher.{Cypher, Neo4jREST}

object LoadLoans extends App {

  println("""
    ___  __     __
   / _ \/ /_ __/ /____
  / ___/ / // / __/ _ \
 /_/  /_/\_,_/\__/\___/

  """)

  Neo4jREST.setServer("localhost")

  val files = new File("data").listFiles
  for (file <- files if file.getName matches "(loans-)\\d{4}.*(-clean.csv)") {
    load(file)
  }
  fixLabels()

  def load(file: File) {
    val query = {
      s"LOAD CSV WITH HEADERS FROM 'file://${file.getAbsolutePath}' AS line" +
      """
      FOREACH(companyNumber IN (CASE WHEN line.benefactorCompanyNumber <> '' THEN [line.benefactorCompanyNumber] ELSE [] END) |
      MERGE (b {companyNumber: companyNumber}) ON CREATE SET
        b.class = line.benefactorClass,
        b.name = line.benefactorName,
        b.benefactorType = line.benefactorType,
        b.benefactorAddress = line.benefactorAddress,
        b.postcode = line.benefactorPostcode
      MERGE (r {name: line.recipientName}) ON CREATE SET
        r.class = line.recipientClass
      CREATE (b)-[d:LOANED_TO {
        ecReference: line.ecReference,
        type: line.type,
        value: toInt(line.value),
        referenceNumber: line.referenceNumber,
        rate: line.rate,
        status: line.status,
        amountRepaid: line.amountRepaid,
        amountConverted: line.amountConverted,
        amountOutstanding: line.amountOutstanding,
        startDate: toInt(replace(line.startDate, '-', '')),
        endDate: toInt(replace(line.endDate, '-', '')),
        repaidDate: toInt(replace(line.repaidDate, '-', '')),
        ecLastNotifiedDate: toInt(replace(line.ecLastNotifiedDate, '-', '')),
        recordedBy: line.recordedBy
      }]->(r)
      )
      FOREACH(name IN (CASE WHEN line.benefactorCompanyNumber = '' THEN [line.benefactorName] ELSE [] END) |
      MERGE (b {name: name}) ON CREATE SET
        b.class = line.benefactorClass,
        b.benefactorType = line.benefactorType
      MERGE (r {name: line.recipientName}) ON CREATE SET
        r.class = line.recipientClass
      CREATE (b)-[d:LOANED_TO {
        ecReference: line.ecReference,
        type: line.type,
        value: toInt(line.value),
        referenceNumber: line.referenceNumber,
        rate: line.rate,
        status: line.status,
        amountRepaid: line.amountRepaid,
        amountConverted: line.amountConverted,
        amountOutstanding: line.amountOutstanding,
        startDate: toInt(replace(line.startDate, '-', '')),
        endDate: toInt(replace(line.endDate, '-', '')),
        repaidDate: toInt(replace(line.repaidDate, '-', '')),
        ecLastNotifiedDate: toInt(replace(line.ecLastNotifiedDate, '-', '')),
        recordedBy: line.recordedBy
      }]->(r)
      )
      """
    }

    println(query)
    val result = Cypher(query).execute()
    if (!result) println(s" => failed to add ${file.getPath}")
  }

  def fixLabels() {
    val individualsQuery = "MATCH (i) WHERE i.class = 'Individual' SET i:Individual REMOVE i.class"
    println(individualsQuery)
    val individualsResult = Cypher(individualsQuery).execute()
    if (!individualsResult) println(s" => failed to run query fixing individual labels")

    val organisationsQuery = "MATCH (o) WHERE o.class = 'Organisation' SET o:Organisation REMOVE o.class"
    println(organisationsQuery)
    val organisationsResult = Cypher(organisationsQuery).execute()
    if (!organisationsResult) println(s" => failed to run query fixing organisation labels")

    val partiesQuery = "MATCH (p) WHERE p.class = 'Party' SET p:Party REMOVE p.class"
    println(partiesQuery)
    val partiesResult = Cypher(partiesQuery).execute()
    if (!partiesResult) println(s" => failed to run query fixing party labels")
  }

}
