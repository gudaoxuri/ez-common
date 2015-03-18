package com.ecfront.common

import org.apache.commons.beanutils.BeanUtilsBean

import scala.annotation.{StaticAnnotation, tailrec}
import scala.collection.mutable.ArrayBuffer
import scala.reflect.runtime.universe._

/**
 * Bean操作辅助类
 */
object BeanHelper {

  val rm = runtimeMirror(getClass.getClassLoader)

  private val copyPropertiesAdapter = new NullAwareBeanUtilsBean

  /**
   * Bean 复制，忽略Null值属性
   * @param dest 目录Bean
   * @param orig 源Bean
   */
  def copyProperties(dest: AnyRef, orig: AnyRef) = copyPropertiesAdapter.copyProperties(dest, orig)

  /**
   * 获取Bean的字段名称及类型
   * @param beanClazz 目标Bean类型
   * @param filterNames 要过滤的名称
   * @param filterAnnotations 要过滤的注解
   */
  def findFields(beanClazz: Class[_], filterNames: Seq[String] = Seq(), filterAnnotations: Seq[Class[_ <: StaticAnnotation]] = Seq(classOf[Ignore])): Map[String, String] = {
    val fields = collection.mutable.Map[String, String]()
    val filter = if (filterAnnotations.nonEmpty) findFieldAnnotations(beanClazz, filterAnnotations) else ArrayBuffer[FieldAnnotationInfo]()
    scala.reflect.runtime.currentMirror.classSymbol(beanClazz).toType.members.collect {
      case method: MethodSymbol if method.isGetter && method.isPublic
        && (filterNames == null || filterNames.isEmpty || !filterNames.contains(method.name.toString.trim)) =>
        if (!filter.exists(_.fieldName == method.name.toString.trim)) {
          fields += (method.name.toString.trim -> method.returnType.toString.trim)
        }
    }
    fields.toMap
  }

  /**
   * 获取Bean中字段的值
   * @param bean 目标Bean
   * @param filterNames 要过滤的名称
   */
  def findValues(bean: AnyRef, filterNames: Seq[String] = Seq()): Map[String, Any] = {
    val fields = collection.mutable.Map[String, Any]()
    val m = rm.reflect(bean)
    scala.reflect.runtime.currentMirror.classSymbol(bean.getClass).toType.members.collect {
      case method: MethodSymbol if method.isGetter && method.isPublic
        && (filterNames == null || filterNames.isEmpty || !filterNames.contains(method.name.toString.trim)) =>
        fields += (method.name.toString.trim -> m.reflectMethod(method).apply())
    }
    fields.toMap
  }

  /**
   * 获取Bean中指定字段的值
   * @param bean 目标Bean
   * @param fieldName 指定字段
   */
  def getValue(bean: AnyRef, fieldName: String): Option[Any] = {
    val m = rm.reflect(bean)
    var value: Any = null
    scala.reflect.runtime.currentMirror.classSymbol(bean.getClass).toType.members.collect {
      case term: TermSymbol if term.name.toString.trim == fieldName =>
        value = m.reflectField(term).get
    }
    Some(value)

  }

  def setValue(bean: AnyRef, fieldName: String, value: Any): Unit = {
    val m = rm.reflect(bean)
    scala.reflect.runtime.currentMirror.classSymbol(bean.getClass).toType.members.collect {
      case term: TermSymbol if term.name.toString.trim == fieldName =>
        m.reflectField(term).set(value)
    }
  }

  /**
   * 递归获取带指定注解的字段
   * @param beanClazz 目标Bean
   * @param annotations 指定的注解，为空时获取所有注解
   * @return 注解信息（注解名称及对应的字段）
   **/
  def findFieldAnnotations(beanClazz: Class[_], annotations: Seq[Class[_ <: StaticAnnotation]] = Seq()): ArrayBuffer[FieldAnnotationInfo] = {
    val result = ArrayBuffer[FieldAnnotationInfo]()
    findFieldAnnotations(result, beanClazz, annotations)
    result
  }

  @tailrec
  private def findFieldAnnotations(container: ArrayBuffer[FieldAnnotationInfo], beanClazz: Class[_], annotations: Seq[Class[_ <: StaticAnnotation]]) {
    scala.reflect.runtime.currentMirror.classSymbol(beanClazz).toType.members.collect {
      case m if !m.isMethod =>
        m.annotations.map {
          annotation =>
            val tmp=annotation.toString
            val annotationName=if(tmp.indexOf("(")== -1) tmp else tmp.substring(0,tmp.lastIndexOf("("))
            if (annotations.isEmpty || annotations.exists(ann => ann.getName ==annotationName)) {
              val value = annotation.tree.children.tail.map(_.productElement(0).asInstanceOf[Constant].value)
              val typeAnnotation = annotation.tree.tpe
              val res = rm.reflectClass(typeAnnotation.typeSymbol.asClass).
                reflectConstructor(typeAnnotation.decl(termNames.CONSTRUCTOR).asMethod)(value: _*)
              container += FieldAnnotationInfo(res, m.name.toString.trim)
            }
        }
    }
    beanClazz.getGenericSuperclass match {
      case c: Class[_] =>
        if (c != classOf[Object]) {
          findFieldAnnotations(container, c, annotations)
        }
    }
  }

  /**
   * 递归获取带指定注解的方法
   * @param beanClazz 目标Bean
   * @param annotations 指定的注解，为空时获取所有注解
   * @return 注解信息（注解名称及对应的方法）
   **/
  def findMethodAnnotations(beanClazz: Class[_], annotations: Seq[Class[_ <: StaticAnnotation]] = Seq()): ArrayBuffer[methodAnnotationInfo] = {
    val result = ArrayBuffer[methodAnnotationInfo]()
    findMethodAnnotations(result, beanClazz, annotations)
    result
  }

  @tailrec
  private def findMethodAnnotations(container: ArrayBuffer[methodAnnotationInfo], beanClazz: Class[_], annotations: Seq[Class[_ <: StaticAnnotation]]) {
    scala.reflect.runtime.currentMirror.classSymbol(beanClazz).toType.members.collect {
      case m if m.isMethod =>
        m.annotations.map {
          annotation =>
            val tmp=annotation.toString
            val annotationName=if(tmp.indexOf("(")== -1) tmp else tmp.substring(0,tmp.lastIndexOf("("))
            if (annotations.isEmpty || annotations.exists(ann => ann.getName ==annotationName)) {
              val value = annotation.tree.children.tail.map(_.productElement(0).asInstanceOf[Constant].value)
              val typeAnnotation = annotation.tree.tpe
              val res = rm.reflectClass(typeAnnotation.typeSymbol.asClass).
                reflectConstructor(typeAnnotation.decl(termNames.CONSTRUCTOR).asMethod)(value: _*)
              container += methodAnnotationInfo(res, m.name.toString.trim)
            }
        }
    }
    beanClazz.getGenericSuperclass match {
      case c: Class[_] =>
        if (c != classOf[Object]) {
          findMethodAnnotations(container, c, annotations)
        }
    }
  }

  def invoke(obj: Any, method: String): MethodMirror = {
    val ref=rm.reflect(obj)
    ref.reflectMethod(ref.symbol.typeSignature.member(TermName(method)).asMethod)
  }

  /**
   * 获取类注解
   * @tparam A 注解类型
   * @param beanClazz 目标类的类型
   * @return 注解对象
   */
  def getClassAnnotation[A: TypeTag](beanClazz: Class[_]): Option[A] = {
    val res = getClassAnnotation(typeOf[A], beanClazz)
    if (res != null) {
      Some(res.get.asInstanceOf[A])
    } else {
      null
    }
  }

  private def getClassAnnotation(typeAnnotation: Type, beanClazz: Class[_]): Option[Any] = {
    var res = scala.reflect.runtime.currentMirror.classSymbol(beanClazz).toType.typeSymbol.asClass.annotations.find(a => a.tree.tpe == typeAnnotation).map {
      annotation =>
        val value = annotation.tree.children.tail.map(_.productElement(0).asInstanceOf[Constant].value)
        rm.reflectClass(typeAnnotation.typeSymbol.asClass).
          reflectConstructor(typeAnnotation.decl(termNames.CONSTRUCTOR).asMethod)(value: _*)
    }
    if (res == None) {
      beanClazz.getGenericSuperclass match {
        case c: Class[_] =>
          if (c != classOf[Object]) {
            res = getClassAnnotation(typeAnnotation, c)
          }
      }
    }
    res
  }
}

private class NullAwareBeanUtilsBean extends BeanUtilsBean {
  override def copyProperty(bean: scala.Any, name: String, value: scala.Any): Unit = {
    if (null != value) {
      super.copyProperty(bean, name, value)
    }
  }
}

case class FieldAnnotationInfo(annotation: Any, fieldName: String)

case class methodAnnotationInfo(annotation: Any, method: String)

@scala.annotation.meta.field
class Ignore extends scala.annotation.StaticAnnotation
