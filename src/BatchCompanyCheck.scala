import java.io.File
import org.anormcypher.{Cypher, Neo4jREST}
import com.github.tototoshi.csv.{CSVReader, CSVWriter}

object BatchNameCheck {

  Neo4jREST.setServer("localhost")

  def run(file: String) {
    val list = CSVReader.open(new File(file)).allWithHeaders
    val results = CSVWriter.open(new File(file.stripSuffix(".csv") + "-found-company-donations.csv"))
    list foreach { entry =>
      val name = entry("Name")
      val row = name :: getDonations(name)
      results.writeRow(row)
    }
  }

  private def getDonations(name: String): List[String] = {
    val cleanName = stripTitles(name).replaceAll("""[()\[\]\*\\'"]""", """\$0""")
    val luceneNameTerms = cleanName.split(" ").filter(_.length >= 2)
    if (luceneNameTerms.isEmpty) List[String]()
    else {
      val luceneName = luceneNameTerms.map("""name:\"""" + _ + """\"""").mkString(" AND ")
      val startCypher = s"START b=node:node_auto_index('$luceneName')"
      val matchCypher = "MATCH (b:Individual)-[iaoo:IS_AN_OFFICER_OF]->(o)-[d:DONATED_TO]->(r)"
      val returnCypher = "RETURN collect(DISTINCT b.name) AS matchedNames, collect(DISTINCT iaoo.position) AS matchedPositions, collect(DISTINCT o.name) AS matchedCompanyNames, collect(DISTINCT o.companyNumber) AS matchedCompanyNumbers, collect(DISTINCT r.name) AS recipients, count(d) AS donationsCount, sum(d.value) / 100.0 AS donationsTotal"
      val result = Cypher(s"$startCypher $matchCypher $returnCypher").apply()
      val donations = result map { row =>
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
      donations.toList.headOption.getOrElse(List[String]())
    }
  }

  private def stripTitles(name: String): String = {
    val prefixes = List("Ms", "Mrs", "Miss", "Mr", "Dr", "Cllr", "Sir", "Dame", "The Hon", "The Rt Hon")
    val suffixes = List("QC", "MP", "MSP", "AM", "MEP")
    val titlesRegex = (prefixes.map("^(" + _ + " )") ++ suffixes.map("( " + _ + ")")).mkString("|")
    name.replaceAll(titlesRegex, "")
  }

}
