package com.ecfront.common

import org.scalatest._

class EncryptHelperSpec extends FunSuite {

  test("加密测试") {
    assert(EncryptHelper.encrypt("gudaoxuri") == "70C0CC2B7BF8A8EBCD7B59C49DDDA9A1E551122BA5D7AB3B7B02141D4CE4C626".toLowerCase)
  }

}

