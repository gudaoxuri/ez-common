package com.ecfront.common

import com.fasterxml.jackson.core.`type`.STypeReference
import com.fasterxml.jackson.databind.`type`.TypeFactory
import org.scalatest.FunSuite

import scala.beans.BeanProperty

class JsonHelperSpec extends FunSuite {

  test("JsonHelper测试") {

    JsonHelper.toJson(Config("aaa"))
    JsonHelper.toJson( """{"a_key":"a_val"}""")
    JsonHelper.toJson( """{"a_key":"a_val"}""")
    print(JsonHelper.toJsonString(JsonHelper.createObjectNode().set("", JsonHelper.createObjectNode().put("a_key", "a_val"))))
    assert(JsonHelper.toObject("1", classOf[Int]) == 1)
    assert(JsonHelper.toObject("""{"name":"sunisle","createTime":123456789,"cid":"1"}""", classOf[TestIdModel]).name=="sunisle")
    val result = JsonHelper.toGenericObject[List[TestIdModel]]( """[{"name":"sunisle","createTime":123456789,"cid":"1"}]""")
    assert(result.head.cid == "1")
    assert(result.head.createTime == 123456789)
    assert(result.head.name == "sunisle")

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

  /*def save[E](objs:String,clazz: Class[E]) {
    val aa=JsonHelper.getMapper.readValue(objs,JsonHelper.getMapper.getTypeFactory().constructCollectionLikeType(classOf[List[_]],classOf[TestIdModel]))
    print(aa)
  }*/

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


