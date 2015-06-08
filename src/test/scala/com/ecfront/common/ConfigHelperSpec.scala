package com.ecfront.common

import com.fasterxml.jackson.databind.JsonNode
import org.scalatest._

class ConfigHelperSpec extends FunSuite {

  test("配置测试") {
    val value1 = ConfigHelper.init[JsonNode](this.getClass.getResource("/config.json").getPath).get.path("loglevel").asText()
    assert(value1 == "DEBUG")
    val value2 = ConfigHelper.init[Config](this.getClass.getResource("/config.json").getPath, classOf[Config]).get
    assert(value2.loglevel == "DEBUG")
    val value3 = ConfigHelper.init[Config](getClass.getResource("/").getPath + "config2.json", classOf[Config]).get
    assert(value3.loglevel == "INFO")
  }
}

case class Config(loglevel: String)


