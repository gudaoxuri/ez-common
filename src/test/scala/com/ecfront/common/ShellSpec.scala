package com.ecfront.common

import org.scalatest.FunSuite


class ShellSpec  extends FunSuite {

  test("Shell测试") {
    //use linux
    assert(ShellHelper.sync("echo hello",returnResult = true)=="echo hello")
  }

}
