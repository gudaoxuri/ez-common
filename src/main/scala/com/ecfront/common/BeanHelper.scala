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
  def getFields(beanClazz: Class[_], filterNames: Seq[String] = Seq(), filterAnnotations: Seq[Class[_ <: StaticAnnotation]] = Seq(classOf[Ignore])): Map[String, String] = {
    val fields = collection.mutable.Map[String, String]()
    val filters = ArrayBuffer[String]()
    if (filterAnnotations != null && filterAnnotations.nonEmpty) {
      getFieldAnnotations(filters, beanClazz, filterAnnotations)
    }
    scala.reflect.runtime.currentMirror.classSymbol(beanClazz).toType.members.collect {
      case m: MethodSymbol if m.isGetter && m.isPublic
        && (filterNames == null || filterNames.isEmpty || !filterNames.contains(m.name))
      =>
        fields += (m.name.toString.trim -> m.returnType.toString.trim)
    }
    fields.filter(f => !filters.contains(f._1)).toMap
  }

  /**
   * 获取Bean中字段的值
   * @param bean 目标Bean
   * @param filterNames 要过滤的名称
   * @param filterAnnotations 要过滤的注解
   */
  def getValues(bean: AnyRef, filterNames: Seq[String] = Seq(), filterAnnotations: Seq[Class[_ <: StaticAnnotation]] = Seq(classOf[Ignore])): Map[String, Any] = {
    if (bean == null) {
      throw new IllegalArgumentException("No bean specified")
    }
    val fields = collection.mutable.Map[String, Any]()
    val filters = ArrayBuffer[String]()
    if (filterAnnotations != null && filterAnnotations.nonEmpty) {
      getFieldAnnotations(filters, bean.getClass, filterAnnotations)
    }
    val instanceMirror = scala.reflect.runtime.currentMirror.reflect(bean)
    scala.reflect.runtime.currentMirror.classSymbol(bean.getClass).toType.members.collect {
      case m: MethodSymbol if m.isGetter && m.isPublic
        && (filterNames == null || filterNames.isEmpty || !filterNames.contains(m.name))
      =>
        fields += (m.name.toString.trim -> instanceMirror.reflectMethod(m).apply())
    }
    fields.filter(f => !filters.contains(f._1)).toMap
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
   * @param container 存放字段容器
   * @param beanClazz 目标Bean
   * @param annotations 要过滤的注解
   */
  @tailrec
  private def getFieldAnnotations(container: ArrayBuffer[String], beanClazz: Class[_], annotations: Seq[Class[_ <: StaticAnnotation]]) {
    scala.reflect.runtime.currentMirror.classSymbol(beanClazz).toType.members.collect {
      case m if !m.isMethod &&
        annotations.exists(ann => m.annotations.exists(ann.getName == _.toString)) =>
        container += m.name.toString.trim
    }
    beanClazz.getGenericSuperclass match {
      case c: Class[_] =>
        if (c != classOf[Object]) {
          getFieldAnnotations(container, c, annotations)
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
      entityClass =>
        val value = entityClass.tree.children.tail.map(_.productElement(0).asInstanceOf[Constant].value)
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

@scala.annotation.meta.field
class Ignore extends scala.annotation.StaticAnnotation
