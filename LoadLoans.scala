import java.io.File
import scala.util.Try
import org.anormcypher.{Cypher, Neo4jREST}

object LoadLoans extends App {

  println("""
    ___  __     __
   / _ \/ /_ __/ /____
  / ___/ / // / __/ _ \
 /_/  /_/\_,_/\__/\___/

  """)

  Neo4jREST.setServer("localhost")

  run()

  def run() {
    val file = new File("loans.csv")
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
    println("Loading loans...")
    Try(Cypher(query).apply()) recover {
      case e => println(s"FAILED TO LOAD LOANS: \n${e.getMessage}")
    }
    fixLabels()
  }

  def fixLabels() {
    val individualsQuery = "MATCH (i) WHERE i.class = 'Individual' SET i:Individual REMOVE i.class"
    val individualsResult = Cypher(individualsQuery).execute()
    if (!individualsResult) println(s"FAILED TO FIX LABELS FOR INDIVIDUALS")

    val organisationsQuery = "MATCH (o) WHERE o.class = 'Organisation' SET o:Organisation REMOVE o.class"
    val organisationsResult = Cypher(organisationsQuery).execute()
    if (!organisationsResult) println(s"FAILED TO FIX LABELS FOR ORGANISATIONS")

    val partiesQuery = "MATCH (p) WHERE p.class = 'Party' SET p:Party REMOVE p.class"
    val partiesResult = Cypher(partiesQuery).execute()
    if (!partiesResult) println(s"FAILED TO FIX LABELS FOR PARTIES")
  }

}
