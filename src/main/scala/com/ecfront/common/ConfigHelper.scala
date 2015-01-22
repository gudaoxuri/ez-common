package com.ecfront.common

import java.io.File

import com.fasterxml.jackson.databind.JsonNode
import com.typesafe.scalalogging.slf4j.LazyLogging

import scala.io.Source

/**
 * 配置文件获取辅助类
 */
object ConfigHelper extends LazyLogging {

  //系统属性名称
  private val SYS_PROP_CONFIG = "config"

  /**
   * 获取属性文件
   * @param configPath  配置文件全路径
   * @param configClazz 要转换成的对象类型，默认为JsonNode
   * @tparam E  要转换成的对象类型
   * @return 配置文件对象
   */
  def init[E]( configPath: String,configClazz: Class[E] = classOf[JsonNode]): Option[E] = {
    val realConfigPath = if (System.getProperties.containsKey(SYS_PROP_CONFIG)) System.getProperty(SYS_PROP_CONFIG) else configPath
    if (new File(realConfigPath).exists()) {
      val configStr = Source.fromFile(realConfigPath).mkString
      val config = if (configClazz.isInstance(classOf[JsonNode])) JsonHelper.toJson(configStr) else JsonHelper.toObject(configStr, configClazz)
      if (config == null) {
        logger.error("The Config [" + realConfigPath + "] parse error!")
        null
      } else {
        logger.info("The Config [" + realConfigPath + "] has bean  initialization!")
        Some(config.asInstanceOf[E])
      }
    } else {
      logger.error("The Config [" + realConfigPath + "] NOT found!")
      null
    }
  }

}

