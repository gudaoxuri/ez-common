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
    JsonHelper.toJson(Config("aaa"))
    JsonHelper.toJson( """{"a_key":"a_val"}""")
    JsonHelper.toJson( """{"a_key":"a_val"}""")
    print(JsonHelper.toJsonString(JsonHelper.createObjectNode().set("", JsonHelper.createObjectNode().put("a_key", "a_val"))))
  }

  test("Bean测试") {
    val fields = BeanHelper.findFields(classOf[TestModel])
    assert(fields.size == 4)
    assert(fields("id") == "String")
    assert(fields("name") == "String")
    assert(fields("bool") == "Boolean")

    val model = TestModel("张三", true, 14)
    model.id = "id001"
    val values = BeanHelper.findValues(model)
    assert(values.size == 6)
    assert(values("id") == "id001")
    assert(values("name") == "张三")
    assert(values("bool") == true)

    BeanHelper.setValue(model,"name","李四")
    assert(model.name=="李四")

    assert(BeanHelper.getValue(model,"name").get=="李四")
    assert(BeanHelper.findValues(model).get("name").get=="李四")

    assert(BeanHelper.getClassAnnotation[Entity](classOf[TestModel]).get.idField == "id")

    val fieldAnnotations = BeanHelper.findFieldAnnotations(classOf[TestModel])
    assert(fieldAnnotations.size == 6)
    assert(fieldAnnotations(0).annotation.isInstanceOf[ManyToMany])
    assert(fieldAnnotations(0).annotation.asInstanceOf[ManyToMany].master)
    assert(!fieldAnnotations(0).annotation.asInstanceOf[ManyToMany].fetch)
    assert(fieldAnnotations(0).fieldName == "relA")
  }
}

case class TestModel(
                      @BeanProperty var name: String,
                      @BeanProperty var bool: Boolean,
                      @Ignore var age: Int
                      ) extends IdModel {
  @ManyToMany(master = true, fetch = false) var relA: List[String] = _
}

case class Test2Model()

@Entity(idField = "id")
abstract class IdModel {
  @BeanProperty var id: String = _
  @Ignore var title: String = _
}


case class Config(loglevel: String)

case class Entity(idField: String) extends StaticAnnotation

@scala.annotation.meta.field
case class ManyToMany(master: Boolean, fetch: Boolean) extends Ignore

