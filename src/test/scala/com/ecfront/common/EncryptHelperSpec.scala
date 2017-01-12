package com.ecfront.common

import org.scalatest._

class EncryptHelperSpec extends FunSuite {

  test("不可逆加密测试") {
    assert(EncryptHelper.encrypt("gudaoxuri") == "70C0CC2B7BF8A8EBCD7B59C49DDDA9A1E551122BA5D7AB3B7B02141D4CE4C626".toLowerCase)
    assert(EncryptHelper.validate("gudaoxuri", EncryptHelper.encrypt("gudaoxuri")))
    assert(EncryptHelper.validate("password", EncryptHelper.encrypt("password", "bcrypt"), "bcrypt"))
  }

  val privateStr="MIICdwIBADANBgkqhkiG9w0BAQEFAASCAmEwggJdAgEAAoGBALjt0CEssHfGENZxyASF6pNtGKYCGW43+LE3JhT8y8TE39vDK22GJZWJFXYfWwasavknIfepBIVrnuMidtcPqUY3bhrDZN+J6MtYaSPSEwRcS2PgF/065CEdSbLy6cvKA64GUiG188un1xIsGBVUdu3fdu41OQvt+90TZT0HclXJAgMBAAECgYEAjXFndVhHCPU3P637PGppBqW06pREeybYUkNKH1dTS4cBaYcXmke2S290OMq2xp3tm++wbUqbKKkt97AOkWNrJfq8Ecpdw9s3c7yQGWaPuwiX38Cgtq6r0utjT20YgR6etGpqafoBt93RZpEm0eEzFPUnS7qYc86HprL0RJ0/i7kCQQDaOmvO82cYIK1ESkA0GdDVQoz2A1V8HvEWOsccRGqlWuasLUccyBnx1G/LDZUxcPOraDyxI8sdl7VbweLR0H9LAkEA2O/rWXwnSYKqdpt+OhpUBHNnMs3IMvRzefJ1zObnIMyYR3KXtpQ/fL4gEquNwJgFIaPJVg5/3zHISEw3e8XOuwJAIDrGl07tZ+vTiyVoLAmwBP8KMH83jdhIBN9zbqJQGdG+Bam+Oer3ofac+CEuapni8uq3I/ZEVj+EomOVKyWe1wJAATztROd2ee7q9h5RDBfWXughsKKH//JxLkL59R9kNkW0oMPApeQWsKmNGU4tUuoLLXP31CvlAusPz4nPzz8DvQJBAJXpICPNJw84fONzS0raRqlFoZMMI0cqeGtPIiCHKaRHyzQv/FFu2KxUcCrod8PngaBFRselzrwZILmXHqrHc1M="
  val publicStr="MIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQC47dAhLLB3xhDWccgEheqTbRimAhluN/ixNyYU/MvExN/bwytthiWViRV2H1sGrGr5JyH3qQSFa57jInbXD6lGN24aw2TfiejLWGkj0hMEXEtj4Bf9OuQhHUmy8unLygOuBlIhtfPLp9cSLBgVVHbt33buNTkL7fvdE2U9B3JVyQIDAQAB"

  test("非对称加密测试") {
    val keys = EncryptHelper.Asymmetric.generateKeys()
    val publicKey = EncryptHelper.Asymmetric.getPublicKey(publicStr)
    val privateKey = EncryptHelper.Asymmetric.getPrivateKey(privateStr)

    val encryptByPub = EncryptHelper.Asymmetric.encrypt("好的了".getBytes("UTF-8"), publicKey)
    val result = new String(EncryptHelper.Asymmetric.decrypt(encryptByPub, privateKey), "UTF-8")
    assert(result == "好的了")

    val encryptByPriv = EncryptHelper.Asymmetric.encrypt("好的了".getBytes("UTF-8"), privateKey)
    val decryptByPub = EncryptHelper.Asymmetric.decrypt(encryptByPriv, publicKey)
    assert(new String(decryptByPub, "UTF-8") == "好的了")
    assert(EncryptHelper.Asymmetric.verify(publicKey, decryptByPub, EncryptHelper.Asymmetric.sign(privateKey, "好的了".getBytes("UTF-8"))))
  }

}

