package com.ecfront.common

import org.scalatest._

class ReqRespVOSpec extends FunSuite {

  test("ReqRespVO测试") {
    val t1 = test1()
    assert(t1.code == StandardCode.SUCCESS)
    val t2 = test2()
    assert(t2.code == StandardCode.BAD_REQUEST)
    val t3 = test3()
    assert(t3.code == StandardCode.SUCCESS)
    val t4 = test4()
    assert(t4.code == StandardCode.BAD_REQUEST)
  }

  def test1(): Resp[Some_Model] = {
    Resp.success(Some_Model())
  }

  def test2(): Resp[Some_Model] = {
    Resp.fail(StandardCode.BAD_REQUEST, "错误")
  }

  def test3(): Resp[Some_Model] = {
    Resp.success(null)
  }

  def test4(): Resp[Some_VO] = {
    test2()
  }

}

