package com.ecfront.common

import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}
import com.fasterxml.jackson.module.scala.DefaultScalaModule

/**
 * Scala版本的Json辅助类<br/>
 * 使用<i>jackson-module-scala</i>封装
 */
object ScalaJsonHelper {

  private val mapper = new ObjectMapper()
  mapper.registerModule(DefaultScalaModule)

  /**
   * object 转 json字符串
   * @param obj object
   * @return json字符串
   */
  def toJsonString(obj: Any): String = {
    mapper.writeValueAsString(obj)
  }

  /**
   * object 转 json
   * @param obj object
   * @return json
   */
  def toJson(obj: Any): JsonNode = {
    obj match {
      case o: String => mapper.readTree(o)
      case _ => mapper.readTree(toJsonString(obj))
    }
  }

  /**
   * json或string 转 object
   * @param obj json或string
   * @return object
   */
  def toObject[E](obj: Any, clazz: Class[E]): E = {
    obj match {
      case o: String => mapper.readValue(o, clazz)
      case o: JsonNode => mapper.readValue(o.toString, clazz)
    }
  }

}
