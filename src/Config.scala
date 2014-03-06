import java.io.File
import com.typesafe.config.ConfigFactory

object Config {

  val config = ConfigFactory.parseFile(new File("plutolatry.conf"))

  val openCorporatesKey = config.getString("openCorporatesKey")
  val donationsData = config.getString("donationsData")
  val loansData = config.getString("loansData")

}
