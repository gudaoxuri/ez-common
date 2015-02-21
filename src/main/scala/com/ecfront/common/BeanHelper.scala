package com.ecfront.common

import org.apache.commons.beanutils.BeanUtilsBean

import scala.annotation.{StaticAnnotation, tailrec}
import scala.collection.mutable.ArrayBuffer
import scala.reflect.runtime.universe._

/**
 * Bean操作辅助类
 */
object BeanHelper {

  val runtime = runtimeMirror(getClass.getClassLoader)

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
    val filter = findFieldAnnotations(beanClazz, filterAnnotations)
    scala.reflect.runtime.currentMirror.classSymbol(beanClazz).toType.members.collect {
      case m: MethodSymbol if m.isGetter && m.isPublic
        && (filterNames == null || filterNames.isEmpty || !filterNames.contains(m.name))
      =>
        if (!filter.exists(_.fieldName == m.name.toString.trim)) {
          fields += (m.name.toString.trim -> m.returnType.toString.trim)
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
    if (bean == null) {
      throw new IllegalArgumentException("No bean specified")
    }
    val fields = collection.mutable.Map[String, Any]()
    val instanceMirror = scala.reflect.runtime.currentMirror.reflect(bean)
    scala.reflect.runtime.currentMirror.classSymbol(bean.getClass).toType.members.collect {
      case m: MethodSymbol if m.isGetter && m.isPublic
        && (filterNames == null || filterNames.isEmpty || !filterNames.contains(m.name))
      =>
        fields += (m.name.toString.trim -> instanceMirror.reflectMethod(m).apply())
    }
    fields.toMap
  }

  /**
   * 获取Bean中指定字段的值
   * @param bean 目标Bean
   * @param fieldName 指定字段
   */
  def getValue(bean: AnyRef, fieldName: String): Option[Any] = {
    if (bean == null) {
      throw new IllegalArgumentException("No bean specified")
    }
    val instanceMirror = scala.reflect.runtime.currentMirror.reflect(bean)
    var value: Any = null
    scala.reflect.runtime.currentMirror.classSymbol(bean.getClass).toType.members.collect {
      case m: MethodSymbol if m.isGetter && m.isPublic && fieldName == m.name
      =>
        value = instanceMirror.reflectMethod(m).apply()
    }
    Some(value)
  }

  /**
   * 递归获取带指定注解的字段
   * @param beanClazz 目标Bean
   * @param annotations 指定的注解，为空时获取所有注解
   **/
  def findFieldAnnotations(beanClazz: Class[_], annotations: Seq[Class[_ <: StaticAnnotation]] = Seq()): ArrayBuffer[AnnotationInfo] = {
    val result = ArrayBuffer[AnnotationInfo]()
    findFieldAnnotations(result, beanClazz, annotations)
    result
  }

  @tailrec
  private def findFieldAnnotations(container: ArrayBuffer[AnnotationInfo], beanClazz: Class[_], annotations: Seq[Class[_ <: StaticAnnotation]]) {
    scala.reflect.runtime.currentMirror.classSymbol(beanClazz).toType.members.collect {
      case m if !m.isMethod && (annotations.isEmpty || annotations.exists(ann => m.annotations.exists(ann.getName == _.toString))) =>
        m.annotations.map {
          annotation =>
            val value = annotation.tree.children.tail.map(_.productElement(0).asInstanceOf[Constant].value)
            val typeAnnotation = annotation.tree.tpe
            val res = runtime.reflectClass(typeAnnotation.typeSymbol.asClass).
              reflectConstructor(typeAnnotation.decl(termNames.CONSTRUCTOR).asMethod)(value: _*)
            container += AnnotationInfo(res, m.name.toString.trim)
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
   * 获取类注解
   * @tparam A 注解类型
   * @param beanClazz 目标类的类型
   * @return 注解对象
   */
  def getClassAnnotation[A: TypeTag](beanClazz: Class[_]): Option[A] = {
    val typeAnnotation = typeOf[A]
    scala.reflect.runtime.currentMirror.classSymbol(beanClazz).toType.typeSymbol.asClass.annotations.find(a => a.tree.tpe == typeAnnotation).map {
      annotation =>
        val value = annotation.tree.children.tail.map(_.productElement(0).asInstanceOf[Constant].value)
        runtime.reflectClass(typeAnnotation.typeSymbol.asClass).
          reflectConstructor(typeAnnotation.decl(termNames.CONSTRUCTOR).asMethod)(value: _*).
          asInstanceOf[A]
    }
  }

}

private class NullAwareBeanUtilsBean extends BeanUtilsBean {
  override def copyProperty(bean: scala.Any, name: String, value: scala.Any): Unit = {
    if (null != value) {
      super.copyProperty(bean, name, value)
    }
  }
}

case class AnnotationInfo(annotation: Any, fieldName: String)

@scala.annotation.meta.field
class Ignore extends scala.annotation.StaticAnnotation
