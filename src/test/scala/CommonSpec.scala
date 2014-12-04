import com.ecfront.common.ConfigHelper
import com.fasterxml.jackson.databind.JsonNode
import org.scalatest._

class CommonSpec extends FunSuite {

  test("配置测试") {
    val value1 = ConfigHelper.init[JsonNode]().get.path("loglevel").asText()
    assert(value1 == "DEBUG")
    val value2 = ConfigHelper.init[Config](classOf[Config]).get
    assert(value2.loglevel == "DEBUG")
    val value3 = ConfigHelper.init[Config](classOf[Config], getClass.getResource("/").getPath + "config2.json").get
    assert(value3.loglevel == "INFO")
  }


}

case class Config(loglevel: String)

