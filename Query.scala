import java.io.File
import scala.io.StdIn
import org.anormcypher.{Cypher, Neo4jREST}
import com.github.tototoshi.csv.{CSVReader, CSVWriter}
import Common._

/*

Querying uses Neo4j legacy indexing for full-text search until something better arrives.
To set it up:

 $ curl -vX POST -H 'Content-Type: application/json' -d '{"name":"node_auto_index","config":{"type":"fulltext","provider":"lucene"}}' localhost:7474/db/data/index/node/
 $ sed -i bak 's/#node_auto_indexing/node_auto_indexing/g' /usr/local/Cellar/neo4j/2.1.1/libexec/conf/neo4j.properties
 $ sed -i bak 's/#node_keys_indexable/node_keys_indexable/g' /usr/local/Cellar/neo4j/2.1.1/libexec/conf/neo4j.properties
 $ neo4j restart
 $ curl -vX POST -H 'Content-Type: application/json' -d '{"query":"MATCH (n) WHERE has(n.name) SET n.name=n.name"}' localhost:7474/db/data/cypher

*/

object Query extends App {

  Neo4jREST.setServer("localhost")

  run()

  def run() {
    val query = StdIn.readLine("Query name:\n=> ") match {
      case "individuals" => queryIndividuals _
      case "organisations" => queryOrganisations _
      case _ => sys.exit()
    }
    val outputFile = StdIn.readLine("Output file:\n=> ")
    val output = CSVWriter.open(new File(outputFile))
    val inputFile = StdIn.readLine("Input file:\n=> ")
    val input = CSVReader.open(new File(inputFile)).allWithHeaders
    input foreach { entry =>
      val name = entry("Name")
      output.writeRow(query(name))
    }
  }

  def luceneName(name: String) = {
    val terms = tidy(stripTitles(name)).split(" ").filter(_.length >= 2)
    terms.map("name:\"" + _ + "\"").mkString(" AND ")
  }

  def queryIndividuals(name: String): List[String] = {
    val query = {
      s"""
        START b=node:node_auto_index('${luceneName(name)}')
        MATCH (b)-[d:DONATED_TO]->(r)
        RETURN
          collect(DISTINCT b.name) AS matchedNames,
          collect(DISTINCT b.companyNumber) AS matchedCompanyNumbers,
          collect(DISTINCT r.name) AS recipients,
          count(d) AS donationsCount,
          sum(d.value) / 100.0 AS donationsTotal
      """
    }
    val results = Cypher(query).apply() flatMap { row =>
      List(
        row[Seq[String]]("matchedNames").mkString("; "),
        row[Seq[String]]("matchedCompanyNumbers").mkString("; "),
        row[Seq[String]]("recipients").mkString("; "),
        row[Int]("donationsCount").toString,
        row[Double]("donationsTotal").toString
      )
    }
    name :: results.toList
  }

  def queryOrganisations(name: String): List[String] = {
    val query = {
      s"""
        START b=node:node_auto_index('${luceneName(name)}')
        MATCH (b:Individual)-[iaoo:IS_AN_OFFICER_OF]->(o)-[d:DONATED_TO]->(r)
        RETURN
          collect(DISTINCT b.name) AS matchedNames,
          collect(DISTINCT iaoo.position) AS matchedPositions,
          collect(DISTINCT o.name) AS matchedCompanyNames,
          collect(DISTINCT o.companyNumber) AS matchedCompanyNumbers,
          collect(DISTINCT r.name) AS recipients,
          count(d) AS donationsCount,
          sum(d.value) / 100.0 AS donationsTotal
        """
    }
    val results = Cypher(query).apply() flatMap { row =>
      List(
        row[Seq[String]]("matchedNames").mkString("; "),
        row[Seq[String]]("matchedPositions").mkString("; "),
        row[Seq[String]]("matchedCompanyNames").mkString("; "),
        row[Seq[String]]("matchedCompanyNumbers").mkString("; "),
        row[Seq[String]]("recipients").mkString("; "),
        row[Int]("donationsCount").toString,
        row[Double]("donationsTotal").toString
      )
    }
    name :: results.toList
  }

}
