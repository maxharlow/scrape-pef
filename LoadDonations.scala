import java.io.File
import scala.util.Try
import org.anormcypher.{Cypher, Neo4jREST}

object LoadDonations extends App {

  println("""
    ___  __     __
   / _ \/ /_ __/ /____
  / ___/ / // / __/ _ \
 /_/  /_/\_,_/\__/\___/

  """)

  Neo4jREST.setServer("localhost")

  run()

  def run() {
    val file = new File("donations.csv")
    val query = {
      s"LOAD CSV WITH HEADERS FROM 'file://${file.getAbsolutePath}' AS line" +
      """
      MERGE (b {name: line.benefactorName, companyNumber: line.benefactorCompanyNumber}) ON CREATE SET
        b.class = line.benefactorClass,
        b.benefactorType = line.benefactorType,
        b.address = line.benefactorAddress,
        b.postcode = line.benefactorPostcode
      MERGE (r {name: line.recipientName}) ON CREATE SET
        r.class = line.recipientClass,
        r.recipientType = line.recipientType,
        r.recipientRegulatedType = line.recipientRegulatedType,
        r.deregisteredDate = line.recipientDeregisteredDate
      CREATE (b)-[d:DONATED_TO {
        ecReference: line.ecReference,
        type: line.type,
        value: toInt(line.value),
        acceptedDate: toInt(replace(line.acceptedDate, '-', '')),
        receivedDate: toInt(replace(line.receivedDate, '-', '')),
        reportedDate: toInt(replace(line.reportedDate, '-', '')),
        nature: line.nature,
        purpose: line.purpose,
        howDealtWith: line.howDealtWith,
        recordedBy: line.recordedBy,
        reportedUnder6212: line.reportedUnder6212,
        isSponsorship: line.isSponsorship
      }]->(r)
      """
    }
    println("Loading donations...")
    Try(Cypher(query).apply()) recover {
      case e => println(s"FAILED TO LOAD DONATIONS: \n${e.getMessage}")
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
