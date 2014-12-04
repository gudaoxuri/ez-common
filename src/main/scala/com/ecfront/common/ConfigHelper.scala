package com.ecfront.common

import java.io.File

import com.fasterxml.jackson.databind.JsonNode
import com.typesafe.scalalogging.slf4j.LazyLogging

import scala.io.Source

object ConfigHelper extends LazyLogging {

  private val SYS_PROP_CONFIG = "config"

  def init[E](configClazz: Class[E] = classOf[JsonNode], configPath: String = getClass.getResource("/").getPath + "config.json"): Option[E] = {
    val realConfigPath = if (System.getProperties.containsKey(SYS_PROP_CONFIG)) System.getProperty(SYS_PROP_CONFIG) + File.pathSeparator + configPath else configPath
    if (new File(realConfigPath).exists()) {
      val configStr = Source.fromFile(realConfigPath).mkString
      val config = if (configClazz.isInstance(classOf[JsonNode])) ScalaJsonHelper.toJson(configStr) else ScalaJsonHelper.toObject(configStr, configClazz)
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

