package com.ecfront.common

import com.fasterxml.jackson.core.`type`.TypeReference
import org.scalatest.FunSuite

import scala.beans.BeanProperty


class JsonHelperSpec extends FunSuite {

  test("JsonHelper测试") {

    JsonHelper.toJson(Config("aaa"))
    JsonHelper.toJson( """{"a_key":"a_val"}""")
    JsonHelper.toJson( """{"a_key":"a_val"}""")
    print(JsonHelper.toJsonString(JsonHelper.createObjectNode().set("", JsonHelper.createObjectNode().put("a_key", "a_val"))))
    assert(JsonHelper.toObject("1", classOf[Int]) == 1)

    val result = JsonHelper.toGenericObject( """[{"name":"sunisle","createTime":123456789,"cid":"1"}]""", new TypeReference[List[TestIdModel]] {})
    assert(result.head.cid == "1")
    assert(result.head.createTime == 123456789)
    assert(result.head.name == "sunisle")
  }

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


