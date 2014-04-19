import scala.util.{Try, Success, Failure}
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import org.json4s.DefaultFormats
import org.json4s.JValue
import org.json4s.native.JsonMethods
import org.anormcypher.Cypher
import CypherTools._
import Utils._

object Members {

  implicit val formats = DefaultFormats

  def run(periodStartDate: DateTime, periodEndDate: DateTime) {
    membersNames foreach { name =>
      println(s"Updating data for $name...")
      memberData(name) match {
        case Failure(e) => println(s" => failed to get data (${e.getMessage.toLowerCase})")
        case Success(memberJson) => {
          val member = getMember(memberJson)
          member.values("name").map(_.init.tail) map { memberName => // unquoted
            val partyName = getPartyName(memberJson)
            val membership = getMembership(memberJson)
            val membershipEndDate = membership.values("endDate") map { dateString =>
              DateTimeFormat.forPattern("yyyyMMdd").parseDateTime(dateString)
            }
            val validMembership = membershipEndDate match {
              case Some(date) if (date isAfter periodStartDate) && (date isBefore periodEndDate) => true
              case None => true
              case _ => false
            }
            if (validMembership) {
              updateMember(member)
              addMembership(membership, memberName, partyName)
            }
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
    nameCheck(name) { memberName =>
      val nameValue = memberName.replaceAll(" ", "%20")
      val parliamentUri = s"http://data.parliament.uk/membersdataplatform/services/mnis/members/query/name*$nameValue/" // todo: add membership=all
      val requestJson = request(parliamentUri) map { response =>
        val cleanResponse = response.replaceAll("[^a-zA-Z0-9 @#{},:\"/._-]", "")
        val memberJson = JsonMethods.parse(cleanResponse) \\ "Member"
        memberJson
      }
      requestJson.filter(!_.children.isEmpty)
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
      "startDate" -> extractString(member \ "HouseStartDate").dropRight(9).date("yyyy-MM-dd"),
      "endDate" -> extractString(member \ "HouseEndDate").dropRight(9).date("yyyy-MM-dd")
    )
  }

  private def addMembership(membership: CypherObject, memberName: String, partyName: String) = {
    val membershipProperties = membership.toMatchString("MEMBER_OF")
    val matchCypher = s"MATCH (p:PoliticalParty), (m {name:'$memberName'}) WHERE p.name =~ '(?i).*$partyName.*'"
    val mergeCypher = s"MERGE (m)-[$membershipProperties]->(p)"
    val result = Cypher(s"$matchCypher $mergeCypher").execute()
    if (!result) println(" => failed to add membership") 
  }

  private def extractString(json: JValue): String = {
    Try {
      json.noNulls.toOption.map(_.extract[String]).getOrElse("")
    }
    match {
      case Failure(e) => ""
      case Success(value) => value
    }
  }

}
