package com.ecfront.common

import com.fasterxml.jackson.databind.JsonNode
import org.scalatest._

import scala.annotation.StaticAnnotation
import scala.beans.BeanProperty

class CommonSpec extends FunSuite {

  test("配置测试") {
    val value1 = ConfigHelper.init[JsonNode](this.getClass.getResource("/config.json").getPath).get.path("loglevel").asText()
    assert(value1 == "DEBUG")
    val value2 = ConfigHelper.init[Config](this.getClass.getResource("/config.json").getPath, classOf[Config]).get
    assert(value2.loglevel == "DEBUG")
    val value3 = ConfigHelper.init[Config](getClass.getResource("/").getPath + "config2.json", classOf[Config]).get
    assert(value3.loglevel == "INFO")
  }

  test("Json测试") {
    val a = JsonHelper.toJson(Config("aaa"))
    val b = JsonHelper.toJson( """{"a_key":"a_val"}""")
    JsonHelper.toJson( """{"a_key":"a_val"}""")
    print(JsonHelper.toJsonString(JsonHelper.createObjectNode().set("", JsonHelper.createObjectNode().put("a_key", "a_val"))))
  }

  test("Bean测试") {
    val fields = BeanHelper.getFields(classOf[TestModel])
    assert(fields.size == 3)
    assert(fields("id") == "String")
    assert(fields("name") == "String")
    assert(fields("bool") == "Boolean")

    val model = TestModel("张三", true, 14)
    model.id = "id001"
    val values = BeanHelper.getValues(model)
    assert(values.size == 3)
    assert(values("id") == "id001")
    assert(values("name") == "张三")
    assert(values("bool") == true)

    assert(BeanHelper.getClassAnnotation[Entity](classOf[TestModel]).get.idField=="id")

  }
}

@Entity(idField = "id")
case class TestModel(
                      @BeanProperty var name: String,
                      @BeanProperty var bool: Boolean,
                      @BeanProperty @Ignore var age: Int
                      ) extends IdModel {
}

abstract class IdModel {
  @BeanProperty var id: String = _
  @BeanProperty
  @Ignore var title: String = _
}


case class Config(loglevel: String)

case class Entity(idField: String) extends StaticAnnotation

