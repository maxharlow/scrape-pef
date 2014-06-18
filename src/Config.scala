import java.io.File
import com.typesafe.config.ConfigFactory

object Config {

  val config = ConfigFactory.parseFile(new File("pluto.conf"))

  val openCorporatesKey = config.getString("openCorporatesKey")
  val dataLocation = config.getString("dataLocation")

}
