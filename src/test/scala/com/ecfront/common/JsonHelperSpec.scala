package com.ecfront.common

import java.util.{Date, TimeZone}

import org.scalatest.FunSuite

import scala.beans.BeanProperty

class JsonHelperSpec extends FunSuite {

  test("JsonHelper测试") {
    JsonHelper.toJson( """{"a_key":"a_val"}""")
    JsonHelper.toJson(
      """
        |{
        |  "a_key":"a_val" // 注释
        |}""".stripMargin)
    print(JsonHelper.toJsonString(JsonHelper.createObjectNode().set("", JsonHelper.createObjectNode().put("a_key", "a_val"))))
    assert(JsonHelper.toObject("1", classOf[Int]) == 1)
    assert(JsonHelper.toObject[Int]("1") == 1)
    assert(JsonHelper.toObject( """{"name":"sunisle","createTime":123456789,"cid":"1"}""", classOf[TestIdModel]).name == "sunisle")
    val result = JsonHelper.toObject[List[TestIdModel]]( """[{"name":"sunisle","createTime":123456789,"cid":"1"}]""")
    assert(result.head.cid == "1")
    assert(result.head.createTime == 123456789)
    assert(result.head.name == "sunisle")

    val json=JsonHelper.createObjectNode()
    json.put("a","aa")
    json.put("b",11)
    assert(json.get("a").asText()=="aa")
    assert(json.path("a").asText()=="aa")
    assert(json.path("aa").asInt(22)==22)

    println(JsonHelper.toObject("""{"date":"2016-07-12T12:00:00"}""",classOf[DateTest]).date)
    JsonHelper.setTimeZone(TimeZone.getTimeZone("Asia/Shanghai"))
    println(JsonHelper.toObject("""{"date":"2016-07-12T12:00:00"}""",classOf[DateTest]).date)

  }

  test("Generic Test") {
    //save("""[{"name":"sunisle","createTime":123456789,"cid":"1"}]""",classOf[List[TestIdModel]])
    /*val methodAnnotations = BeanHelper.findMethodAnnotations(classOf[CenericClass], Seq(classOf[Get]))
    val tt = methodAnnotations(0).method.paramLists(0)(0).typeSignature
   // val result = JsonHelper.getMapper.readValue( """[{"name":"sunisle","createTime":123456789,"cid":"1"}]""", new STypeReference(tt))
    classOf[CenericClass].getMethods.foreach{
      m =>
        if (m.getName=="save"){
          JsonHelper.getMapper
          val result = JsonHelper.getMapper.readValue( """[{"name":"sunisle","createTime":123456789,"cid":"1"}]""", new STypeReference(m.getGenericParameterTypes.toList(0)))
          result
        }
    }*/
  }


}

class DateTest{
  @BeanProperty var date:Date=_
}

trait Id {
  @BeanProperty var cid: String = _
}

trait Ext extends Id {
  @BeanProperty var createTime: Long = _
}

case class TestIdModel() extends Ext {
  @BeanProperty var name: String = _
}


