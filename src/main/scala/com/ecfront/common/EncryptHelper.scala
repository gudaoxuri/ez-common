package com.ecfront.common

import java.security._
import java.security.spec.{PKCS8EncodedKeySpec, X509EncodedKeySpec}
import java.util.Base64
import javax.crypto.Cipher

import com.github.t3hnar.bcrypt._
import com.typesafe.scalalogging.slf4j.LazyLogging

object EncryptHelper extends LazyLogging {

  /**
    * 对称加密
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

  /**
    * Base64 转 数组
    */
  def decodeBase64ToBytes(str: String): Array[Byte] = {
    Base64.getDecoder.decode(str)
  }

  /**
    * Base64 转 字符串
    */
  def decodeBase64ToString(str: String, encode: String = "UTF-8"): String = {
    new String(Base64.getDecoder.decode(str), encode)
  }

  /**
    * 数组  转 Base64
    */
  def encodeBytesToBase64(str: Array[Byte], encode: String = "UTF-8"): String = {
    new String(Base64.getEncoder.encode(str), encode)
  }

  /**
    * 字符串 转 Base64
    */
  def encodeStringToBase64(str: String, encode: String = "UTF-8"): String = {
    new String(Base64.getEncoder.encode(str.getBytes(encode)), encode)
  }

  private def bytes2hex(bytes: Array[Byte], sep: Option[String] = None): String = {
    sep match {
      case None => bytes.map("%02x".format(_)).mkString
      case _ => bytes.map("%02x".format(_)).mkString(sep.get)
    }
  }

  /**
    * 非对称加密
    */
  object Asymmetric {

    /**
      * 生成公钥和私钥
      *
      * @param algorithm 非对称算法
      * @param length    密钥长度
      * @param encode    转Base64的编码
      * @return PublicKey -> Base64编码后的值  ， PrivateKey -> Base64编码后的值
      */
    def generateKeys(algorithm: String = "RSA", length: Int = 1024, encode: String = "UTF-8"): Map[String, String] = {
      val keyPairGenerator = KeyPairGenerator.getInstance(algorithm)
      keyPairGenerator.initialize(length)
      val keyPair = keyPairGenerator.generateKeyPair()
      Map(
        "PublicKey" -> EncryptHelper.encodeBytesToBase64(keyPair.getPublic.getEncoded, encode),
        "PrivateKey" -> EncryptHelper.encodeBytesToBase64(keyPair.getPrivate.getEncoded, encode)
      )
    }

    /**
      * 获取私钥文件
      *
      * @param key       Base64编码的私钥
      * @param algorithm 非对称算法
      * @return 私钥文件
      */
    def getPrivateKey(key: String, algorithm: String = "RSA"): PrivateKey = {
      val privateKey = EncryptHelper.decodeBase64ToBytes(key)
      val spec = new PKCS8EncodedKeySpec(privateKey)
      KeyFactory.getInstance(algorithm).generatePrivate(spec)
    }

    /**
      * 获取公钥文件
      *
      * @param key       Base64编码的公钥
      * @param algorithm 非对称算法
      * @return 公钥文件
      */
    def getPublicKey(key: String, algorithm: String = "RSA"): PublicKey = {
      val privateKey = EncryptHelper.decodeBase64ToBytes(key)
      val spec = new X509EncodedKeySpec(privateKey)
      KeyFactory.getInstance(algorithm).generatePublic(spec)
    }

    /**
      * 加密
      *
      * @param data      要加密的数据
      * @param key       公钥或私钥文件
      * @param algorithm 非对称算法
      * @return 加密后的数据
      */
    def encrypt(data: Array[Byte], key: Key, algorithm: String = "RSA"): Array[Byte] = {
      val cipher = Cipher.getInstance(algorithm)
      cipher.init(Cipher.ENCRYPT_MODE, key)
      cipher.doFinal(data)
    }

    /**
      * 解密
      *
      * @param data      要解密的数据
      * @param key       公钥或私钥文件
      * @param algorithm 非对称算法
      * @return 解密后的数据
      */
    def decrypt(data: Array[Byte], key: Key, algorithm: String = "RSA"): Array[Byte] = {
      val cipher = Cipher.getInstance(algorithm)
      cipher.init(Cipher.DECRYPT_MODE, key)
      cipher.doFinal(data)
    }

    /**
      * 签名
      *
      * @param key       私钥文件
      * @param data      要签名的数据
      * @param algorithm 签名算法（如SHA1withRSA、MD5withRSA）
      * @return 签名数据
      */
    def sign(key: PrivateKey, data: Array[Byte], algorithm: String = "SHA1withRSA"): Array[Byte] = {
      val signer = Signature.getInstance(algorithm)
      signer.initSign(key)
      signer.update(data)
      signer.sign
    }

    /**
      * 验证
      *
      * @param key       公钥文件
      * @param data      解密后的数据
      * @param signature 签名数据
      * @param algorithm 签名算法（如SHA1withRSA、MD5withRSA）
      * @return 是否通过
      */
    def verify(key: PublicKey, data: Array[Byte], signature: Array[Byte], algorithm: String = "SHA1withRSA"): Boolean = {
      val verifier = Signature.getInstance(algorithm)
      verifier.initVerify(key)
      verifier.update(data)
      verifier.verify(signature)
    }

  }

}
