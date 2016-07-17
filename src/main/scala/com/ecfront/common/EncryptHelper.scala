package com.ecfront.common

import java.security.MessageDigest

import com.github.t3hnar.bcrypt._
import com.typesafe.scalalogging.slf4j.LazyLogging

object EncryptHelper extends LazyLogging {

  /**
    * 加密
    *
    * @param strSrc    原始值
    * @param algorithm 加密算法
    * @return 加密后的值
    */
  def encrypt(strSrc: String, algorithm: String = "SHA-256"): String = {
    algorithm.toLowerCase() match {
      case "bcrypt" =>
        strSrc.bcrypt
      case _ =>
        val md = MessageDigest.getInstance(algorithm)
        md.digest(strSrc.getBytes)
          .foldLeft("")((s: String, b: Byte) => s +
            Character.forDigit((b & 0xf0) >> 4, 16) +
            Character.forDigit(b & 0x0f, 16))
    }
  }

  /**
    * 验证
    *
    * 对于bcrypt之类默认使用随机slat的加密算法必须使用此方法验证
    *
    * @param strSrc       原始值
    * @param strEncrypted 加密后的值
    * @param algorithm    加密算法
    * @return 是否匹配
    */
  def validate(strSrc: String, strEncrypted: String, algorithm: String = "SHA-256"): Boolean = {
    algorithm.toLowerCase() match {
      case "bcrypt" =>
        strSrc.isBcrypted(strEncrypted)
      case _ =>
        encrypt(strSrc, algorithm) == strEncrypted
    }
  }

  private def bytes2hex(bytes: Array[Byte], sep: Option[String] = None): String = {
    sep match {
      case None => bytes.map("%02x".format(_)).mkString
      case _ => bytes.map("%02x".format(_)).mkString(sep.get)
    }
  }

}
