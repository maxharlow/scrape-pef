import scala.util.Try
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.immutable.ListMap
import dispatch._
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import org.json4s.DefaultFormats
import org.json4s.{JValue, JString, JNull}
import org.json4s.native.JsonMethods
import org.anormcypher.{Cypher, Neo4jREST}
import Common._

object LinkMembers extends App {

  implicit val formats = DefaultFormats

  val http = Http configure { b =>
    b.setMaxRequestRetry(5)
  }

  println("""
    ___  __     __
   / _ \/ /_ __/ /____
  / ___/ / // / __/ _ \
 /_/  /_/\_,_/\__/\___/

  """)

  Neo4jREST.setServer("localhost")

  run()

  def run() {
    for (memberName <- membersNames)
    yield for (response <- retrieve(memberName)) {
      val member = selectMember(response)
      val membership = selectMembership(response)
      val partyName = selectPartyName(response)
      load(memberName, member, membership, partyName)
    }
  }

  def membersNames: List[String] = {
    val query = Cypher("MATCH m WHERE m.recipientRegulatedType='MP - Member of Parliament' RETURN m.name AS name").apply()
    query.map(_[String]("name")).toList
  }

  def retrieve(memberName: String): Try[JValue] = {
    val nameValue = memberName.replaceAll(" ", "%20")
    val response = Try {
      val location = url(s"http://data.parliament.uk/membersdataplatform/services/mnis/members/query/membership=all%7Cname*$nameValue/")
      http(location.setHeader("Content-Type", "application/json") OK as.String).apply() match {
        case r if r.length <= 20 => throw new Exception("NOTFOUND")
        case r => r
      }
    }
    response recover {
      case e if e.getMessage == "NOTFOUND" => println(s"MEMBER NOT FOUND: $memberName")
      case e => e.printStackTrace
    }
    response map { r =>
      val cleanResponse = r.replaceAll("[^a-zA-Z0-9 @#{},:\"/._-]", "")
      JsonMethods.parse(cleanResponse) \\ "Member"
    }
  }

  def selectMember(member: JValue): ListMap[String, JValue] = {
    ListMap(
      "constituency" -> member \ "MemberFrom",
      "house" -> member \ "House"
    )
  }

  def selectMembership(member: JValue): ListMap[String, JValue] = {
    def stripTime(datetime: JValue) = datetime match {
      case JString(s) => JString(s dropRight 9)
      case x => JNull // actual response is inexplicably an object containing a null...
    }
    ListMap(
      "startDate" -> stripTime(member \ "HouseStartDate"),
      "endDate" -> stripTime(member \ "HouseEndDate")
    )
  }

  def selectPartyName(member: JValue): String = {
     (member \ "Party" \ "#text").extract[String]
  }

  def load(memberName: String, member: ListMap[String, JValue], membership: ListMap[String, JValue], partyName: String): Unit = {
    val memberProps = propertise("m", member)
    val membershipProps = propertise("mbshp", membership)
    val membershipStartDate = membership("startDate").extract[String].replaceAll("-", "")
    val query = {
      s"""
        MATCH (m:Individual {name:'$memberName'}) SET $memberProps
        WITH m
        MATCH (p:Party) WHERE p.name =~ '$partyName.*'
        MERGE (m)-[mbshp:IS_A_MP_FOR {startDate: $membershipStartDate}]-(p) ON CREATE SET $membershipProps
      """
    }
    Try(Cypher(query).apply()) recover {
      case e => println(s"FAILED TO UPDATE MEMBER: $memberName \n${e.getMessage}")
    }
  }

}
