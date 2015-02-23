package com.ecfront.common

import java.security.MessageDigest

import com.typesafe.scalalogging.slf4j.LazyLogging

object EncryptHelper extends LazyLogging {

  /**
   * 加密
   *
   * @param strSrc 原始值
   * @param algorithm 加密算法
   * @return 加密后的值
   */
  def encrypt(strSrc: String, algorithm: String = "SHA-256"): String = {
    val md = MessageDigest.getInstance(algorithm)
    md.digest(strSrc.getBytes)
      .foldLeft("")((s: String, b: Byte) => s +
      Character.forDigit((b & 0xf0) >> 4, 16) +
      Character.forDigit(b & 0x0f, 16))
  }

}
