package com.ecfront.common

import org.scalatest._

import scala.annotation.StaticAnnotation
import scala.beans.BeanProperty

class BeanHelperSpec extends FunSuite {

  test("Bean测试") {
    val fields = BeanHelper.findFields(classOf[TestModel])
    assert(fields.size == 4)
    assert(fields("id") == "String")
    assert(fields("name") == "String")
    assert(fields("bool") == "Boolean")

    val model = TestModel("张三", bool = true, 14)
    model.id = "id001"
    val values = BeanHelper.findValues(model)
    assert(values.size == 6)
    assert(values("id") == "id001")
    assert(values("name") == "张三")
    assert(values("bool") == true)

    BeanHelper.setValue(model, "name", "李四")
    assert(model.name == "李四")

    assert(BeanHelper.getValue(model, "name").get == "李四")
    assert(BeanHelper.findValues(model).get("name").get == "李四")

    assert(BeanHelper.getClassAnnotation[Entity](classOf[TestModel]).get.idField == "id")

    val fieldAnnotations = BeanHelper.findFieldAnnotations(classOf[TestModel])
    assert(fieldAnnotations.size == 6)
    assert(fieldAnnotations.head.annotation.isInstanceOf[ManyToMany])
    assert(fieldAnnotations.head.annotation.asInstanceOf[ManyToMany].master)
    assert(!fieldAnnotations.head.annotation.asInstanceOf[ManyToMany].fetch)
    assert(fieldAnnotations.head.fieldName == "relA")

    val methodObjectAnnotations = BeanHelper.findMethodAnnotations(Test2Model.getClass)
    assert(methodObjectAnnotations.size == 1)

    val methodAnnotations = BeanHelper.findMethodAnnotations(classOf[TestModel], Seq(classOf[Get]))
    assert(BeanHelper.invoke(model, methodAnnotations.head.method)(10, 2) == 5)
    assert(BeanHelper.invoke(model, methodAnnotations(1).method)(10, 2) == 20)
  }

  test("Bean copy测试") {
    val model1 = Some_Model()
    model1.name = "0"
    model1.only_model = "m"
    val vo1 = Some_VO()
    BeanHelper.copyProperties(vo1, model1)
    assert(vo1.name == "0")
    val vo2 = Some_VO()
    vo2.name = "0"
    vo2.only_vo = "v"
    val model2 = Some_Model()
    BeanHelper.copyProperties(model2, vo2)
    assert(model2.name == "0")
  }

}

case class Some_Model() {
  @BeanProperty var name: String = _
  @BeanProperty var only_model: String = _
}

case class Some_VO() {
  @BeanProperty var name: String = _
  @BeanProperty var only_vo: String = _
}

object Test2Model {
  @Get(url = "/multiply/")
  def multiply(x: Int, y: Int) = x * y
}

case class TestModel(
                      @BeanProperty var name: String,
                      @BeanProperty var bool: Boolean,
                      @Ignore var age: Int
                      ) extends IdModel {
  @ManyToMany(master = true, fetch = false) var relA: List[String] = _

  @Get(url = "/multiply/")
  def multiply(x: Int, y: Int) = x * y

  @Get(url = "/divide/")
  @Ignore
  def divide(x: Int, y: Int) = x / y
}

case class Test2Model()

@Entity(idField = "id")
abstract class IdModel {
  @BeanProperty var id: String = _
  @Ignore var title: String = _
}

case class Entity(idField: String) extends StaticAnnotation

@scala.annotation.meta.field
case class ManyToMany(master: Boolean, fetch: Boolean) extends Ignore

case class Get(url: String) extends StaticAnnotation

