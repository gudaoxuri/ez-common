package com.ecfront.common

import org.scalatest._

import scala.annotation.StaticAnnotation

class ClassScanHelperSpec extends FunSuite {

  test("Class扫描测试") {
    val resultInFile = ClassScanHelper.scan[Scan]("com.ecfront.common")
    assert(resultInFile.size == 2)
    assert(resultInFile.head.getSimpleName == "ClassScanTestA$")
    assert(resultInFile(1).getSimpleName == "ClassScanTestB")
    val resultInFile2 = ClassScanHelper.scan[Scan]("com.ecfront.common", ignoreInterface = false)
    assert(resultInFile2.size == 3)
    assert(resultInFile2.head.getSimpleName == "ClassScanTestA$")
    assert(resultInFile2(1).getSimpleName == "ClassScanTestInterface")
    assert(resultInFile2(2).getSimpleName == "ClassScanTestB")
    val resultInJar = ClassScanHelper.scan[scala.annotation.implicitNotFound]("scala", ignoreInterface = false)
    //e.g. trait ExecutionContext
    assert(resultInJar.nonEmpty)
  }

}

@Scan("")
object ClassScanTestA

@Scan("")
trait ClassScanTestInterface

case class Scan(url: String) extends StaticAnnotation

