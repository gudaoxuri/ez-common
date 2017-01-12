package com.ecfront.common

object AnnotationSpec extends App{

  import scala.annotation.StaticAnnotation
  import scala.reflect.runtime._
  import scala.reflect.runtime.universe._

  case class Table(idField: String = "") extends StaticAnnotation
  @Table()
  case class SomeEntity()

  println(getClassAnnotation[Table](classOf[SomeEntity]).idField)

  def getClassAnnotation[A: TypeTag](beanClazz: Class[_]): A = {
    val typeAnnotation=currentMirror.typeOf[A]
    currentMirror.classSymbol(beanClazz).toType.typeSymbol.asClass.annotations.find(a => a.tree.tpe == typeAnnotation).map {
      annotation =>
        val value = annotation.tree.children.tail.map(_.productElement(0).asInstanceOf[Constant].value)
        currentMirror.reflectClass(typeAnnotation.typeSymbol.asClass).
          reflectConstructor(typeAnnotation.decl(termNames.CONSTRUCTOR).asMethod)(value: _*)
    }.get.asInstanceOf[A]
  }

}


