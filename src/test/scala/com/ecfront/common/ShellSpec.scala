package com.ecfront.common

import org.scalatest.FunSuite


class ShellSpec  extends FunSuite {

  test("Shell测试") {
    assert(ShellHelper.sync("echo hello",true)=="echo hello")
  }

}
