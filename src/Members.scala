import scala.util.{Try, Success, Failure}
import scalaj.http.{Http, HttpOptions}
import org.json4s.DefaultFormats
import org.json4s.JValue
import org.json4s.native.JsonMethods
import org.anormcypher.Cypher
import CypherTools._

object Members {

  implicit val formats = DefaultFormats

  def run() {
    membersNames foreach { name =>
      println(s"Updating data for $name...")
      memberData(name) match {
        case Failure(e) => println(s" => failed to get data (${e.getMessage.toLowerCase})")
        case Success(memberJson) => {
          if (memberJson.children.isEmpty) println(" => failed to find member")
          val member = getMember(memberJson)
          member.values("name").map(_.init.tail) map { memberName => // unquoted
            updateMember(member)
            val partyName = getPartyName(memberJson)
            val membership = getMembership(memberJson)
            addMembership(membership, memberName, partyName)
          }
        }
      }
    }
  }

  private def membersNames: List[String] = {
    val membersNamesQuery = Cypher("MATCH m WHERE m.recipientRegulatedType='MP - Member of Parliament' RETURN m.name AS name").apply()
    membersNamesQuery.map(_[String]("name")).toList
  }

  private def memberData(name: String): Try[JValue] = {
    val nameValue = name.replaceAll(" ", "%20")
    val parliamentResponse = Try {
      Http(s"http://data.parliament.uk/membersdataplatform/services/mnis/members/query/name*$nameValue/")
        .header("Content-Type", "application/json")
        .option(HttpOptions.connTimeout(5000))
        .option(HttpOptions.readTimeout(7000)).asString
    }
    parliamentResponse map { response =>
      val cleanResponse = response.replaceAll("[^a-zA-Z0-9 @#{},:\"/._-]", "")
      JsonMethods.parse(cleanResponse) \\ "Member"
    }
  }

  private def getMember(member: JValue): CypherObject = {
    new CypherObject(
      "name" -> extractString(member \ "DisplayAs").string,
      "constituency" -> extractString(member \ "MemberFrom").string,
      "house" -> extractString(member \ "House").string
    )
  }

  private def updateMember(member: CypherObject): Unit = {
    member.values("name") map { memberName =>
      val memberProperties = member.toUpdateString("m")
      val result = Cypher(s"MATCH (m {name:$memberName}) SET $memberProperties").execute()
      if (!result) println(" => failed to add member details")
    }
  }

  private def getPartyName(member: JValue): String = {
    extractString(member \ "Party" \ "#text")
  }

  private def getMembership(member: JValue): CypherObject = {
    new CypherObject(
      "startDate" -> extractString(member \ "HouseStartDate").dropRight(9).date("yyyy-MM-dd")
    )
  }

  private def addMembership(membership: CypherObject, memberName: String, partyName: String) = {
    val membershipProperties = membership.toMatchString("MEMBER_OF")
    val matchCypher = s"MATCH (p:`Political Party`), (m {name:'$memberName'}) WHERE p.name =~ '(?i).*$partyName.*'"
    val mergeCypher = s"MERGE (m)-[$membershipProperties]->(p)"
    val result = Cypher(s"$matchCypher $mergeCypher").execute()
    if (!result) println(" => failed to add membership") 
  }

  private def extractString(json: JValue): String = {
    json.noNulls.toOption.map(_.extract[String]).getOrElse("")
  }

}
