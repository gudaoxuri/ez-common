package com.ecfront.common

import org.apache.commons.beanutils.BeanUtilsBean

import scala.annotation.{StaticAnnotation, tailrec}
import scala.collection.mutable.ArrayBuffer
import scala.reflect.runtime.universe._

/**
  * Bean操作辅助类
  */
object BeanHelper {

  private val rm = runtimeMirror(getClass.getClassLoader)

  private val copyPropertiesAdapter = new NullAwareBeanUtilsBean

  /**
    * Bean 复制，忽略Null值属性
    *
    * @param dest 目录Bean
    * @param orig 源Bean
    */
  def copyProperties(dest: AnyRef, orig: AnyRef) = copyPropertiesAdapter.copyProperties(dest, orig)

  /**
    * 获取Bean的字段名称及类型
    *
    * @param beanClazz          目标Bean类型
    * @param excludeNames       要排除的名称，默认为空
    * @param excludeAnnotations 要排除的注解，默认为 Ignore
    * @param includeNames       要包含的名称，默认为全部
    * @param includeAnnotations 要包含的注解，默认为全部
    */
  def findFields(beanClazz: Class[_],
                 excludeNames: Seq[String] = Seq(),
                 excludeAnnotations: Seq[Class[_ <: StaticAnnotation]] = Seq(classOf[Ignore]),
                 includeNames: Seq[String] = Seq(),
                 includeAnnotations: Seq[Class[_ <: StaticAnnotation]] = Seq()
                ): Map[String, String] = {
    val fields = collection.mutable.Map[String, String]()
    val includeAnnotationFields =
      if (includeAnnotations == null || includeAnnotations.isEmpty)
        ArrayBuffer[String]()
      else
        findFieldAnnotations(beanClazz, includeAnnotations).map(_.fieldName)
    val excludeAnnotationFields =
      if (excludeAnnotations == null || excludeAnnotations.isEmpty)
        ArrayBuffer[String]()
      else
        findFieldAnnotations(beanClazz, excludeAnnotations).map(_.fieldName)

    scala.reflect.runtime.currentMirror.classSymbol(beanClazz).toType.members.collect {
      case method: MethodSymbol if method.isGetter && method.isPublic =>
        val methodName = method.name.toString.trim
        if ((includeNames == null || includeNames.isEmpty || includeNames.contains(methodName)) &&
          (excludeNames == null || excludeNames.isEmpty || !excludeNames.contains(methodName)) &&
          (excludeNames == null || excludeNames.isEmpty || !excludeNames.contains(methodName)) &&
          (includeAnnotationFields.isEmpty || includeAnnotationFields.contains(methodName)) &&
          (excludeAnnotationFields.isEmpty || !excludeAnnotationFields.contains(methodName))
        ) {
          fields += (method.name.toString.trim -> method.returnType.toString.trim)
        }
    }
    fields.toMap
  }

  /**
    * 获取Bean中字段的值
    *
    * @param bean         目标Bean
    * @param excludeNames 要排除的名称，默认为空
    * @param includeNames 要包含的名称，默认为全部
    */
  def findValues(bean: AnyRef, excludeNames: Seq[String] = Seq(), includeNames: Seq[String] = Seq()): Map[String, Any] = {
    val fields = collection.mutable.Map[String, Any]()
    val m = rm.reflect(bean)
    scala.reflect.runtime.currentMirror.classSymbol(bean.getClass).toType.members.collect {
      case method: MethodSymbol if method.isGetter && method.isPublic =>
        val methodName = method.name.toString.trim
        if ((includeNames == null || includeNames.isEmpty || includeNames.contains(methodName)) &&
          (excludeNames == null || excludeNames.isEmpty || !excludeNames.contains(methodName)) &&
          (excludeNames == null || excludeNames.isEmpty || !excludeNames.contains(methodName))
        ) {
          fields += (method.name.toString.trim -> m.reflectMethod(method).apply())
        }
    }
    fields.toMap
  }

  /**
    * 获取Bean中指定字段的值
    *
    * @param bean      目标Bean
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

  /**
    * 设置Bean中指定字段的值
    *
    * @param bean      目标Bean
    * @param fieldName 指定字段
    * @param value     值
    */
  def setValue(bean: AnyRef, fieldName: String, value: Any): Unit = {
    val m = rm.reflect(bean)
    scala.reflect.runtime.currentMirror.classSymbol(bean.getClass).toType.members.collect {
      case term: TermSymbol if term.name.toString.trim == fieldName =>
        m.reflectField(term).set(value)
    }
  }


  /**
    * 执行Bean中指定的方法
    *
    * @param bean   目标Bean
    * @param method 指定方法
    * @return 结果
    */
  def invoke(bean: Any, method: MethodSymbol): MethodMirror = {
    rm.reflect(bean).reflectMethod(method)
  }

  /**
    * 递归获取带指定注解的字段
    *
    * @param beanClazz   目标Bean
    * @param annotations 指定的注解，为空时获取所有注解
    * @return 注解信息（注解名称及对应的字段）
    **/
  def findFieldAnnotations(beanClazz: Class[_], annotations: Seq[Class[_ <: StaticAnnotation]] = Seq()): ArrayBuffer[FieldAnnotationInfo] = {
    val result = ArrayBuffer[FieldAnnotationInfo]()
    findFieldAnnotations(result, beanClazz, annotations)
    result
  }

  private def findFieldAnnotations(container: ArrayBuffer[FieldAnnotationInfo], beanClazz: Class[_], annotations: Seq[Class[_ <: StaticAnnotation]]) {
    scala.reflect.runtime.currentMirror.classSymbol(beanClazz).toType.members.collect {
      case m if !m.isMethod =>
        m.annotations.map {
          annotation =>
            val tmp = annotation.toString
            val annotationName = if (tmp.indexOf("(") == -1) tmp else tmp.substring(0, tmp.lastIndexOf("("))
            if (annotations.isEmpty || annotations.exists(ann => ann.getName == annotationName)) {
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
      case _ =>
    }
    beanClazz.getGenericInterfaces.foreach {
      case c: Class[_] =>
        if (c != classOf[Object]) {
          findFieldAnnotations(container, c, annotations)
        }
      case _ =>
    }
  }

  /**
    * 递归获取带指定注解的方法，当beanClazz 为object 时务必使用 getClass 获取
    *
    * @param beanClazz   目标Bean
    * @param annotations 指定的注解，为空时获取所有注解
    * @return 注解信息（注解名称及对应的方法）
    **/
  def findMethodAnnotations(beanClazz: Class[_], annotations: Seq[Class[_ <: StaticAnnotation]] = Seq()): ArrayBuffer[methodAnnotationInfo] = {
    val result = ArrayBuffer[methodAnnotationInfo]()
    findMethodAnnotations(result, beanClazz, annotations)
    result
  }

  private def findMethodAnnotations(container: ArrayBuffer[methodAnnotationInfo], beanClazz: Class[_], annotations: Seq[Class[_ <: StaticAnnotation]]) {
    val tf = scala.reflect.runtime.currentMirror.classSymbol(beanClazz).toType
    tf.members.collect {
      case m if m.isMethod =>
        m.annotations.map {
          annotation =>
            val tmp = annotation.toString
            if (!tmp.startsWith("throws[java.")) {
              val annotationName = if (tmp.indexOf("(") == -1) tmp else tmp.substring(0, tmp.lastIndexOf("("))
              if (annotations.isEmpty || annotations.exists(ann => ann.getName == annotationName)) {
                val value = annotation.tree.children.tail.map(_.productElement(0).asInstanceOf[Constant].value)
                val typeAnnotation = annotation.tree.tpe
                val ann = rm.reflectClass(typeAnnotation.typeSymbol.asClass).
                  reflectConstructor(typeAnnotation.decl(termNames.CONSTRUCTOR).asMethod)(value: _*)
                container += methodAnnotationInfo(ann, tf.member(TermName(m.name.toString.trim)).asMethod)
              }
            }
        }
    }
    beanClazz.getGenericSuperclass match {
      case c: Class[_] =>
        if (c != classOf[Object]) {
          findMethodAnnotations(container, c, annotations)
        }
      case _ =>
    }
    beanClazz.getGenericInterfaces.foreach {
      case c: Class[_] =>
        if (c != classOf[Object]) {
          findMethodAnnotations(container, c, annotations)
        }
      case _ =>
    }
  }

  /**
    * 获取类注解
    *
    * @tparam A 注解类型
    * @param beanClazz 目标类的类型
    * @return 注解对象
    */
  def getClassAnnotation[A: TypeTag](beanClazz: Class[_]): Option[A] = {
    val res = getClassAnnotation(typeOf[A], beanClazz)
    if (res.isDefined) {
      Some(res.get.asInstanceOf[A])
    } else {
      None
    }
  }

  private def getClassAnnotation(typeAnnotation: Type, beanClazz: Class[_]): Option[Any] = {
    var res = scala.reflect.runtime.currentMirror.classSymbol(beanClazz).toType.typeSymbol.asClass.annotations.find(a => a.tree.tpe == typeAnnotation).map {
      annotation =>
        val value = annotation.tree.children.tail.map(_.productElement(0).asInstanceOf[Constant].value)
        rm.reflectClass(typeAnnotation.typeSymbol.asClass).
          reflectConstructor(typeAnnotation.decl(termNames.CONSTRUCTOR).asMethod)(value: _*)
    }
    if (res.isEmpty) {
      beanClazz.getGenericSuperclass match {
        case c: Class[_] =>
          if (c != classOf[Object]) {
            res = getClassAnnotation(typeAnnotation, c)
          }
        case _ =>
      }
    }
    res
  }

  /**
    * 根据class类名字符串生成对应的class类
    *
    * @param clazzStr 类名字符串
    * @return 对应的class类
    */
  def getClassByStr(clazzStr: String): Class[_] = {
    clazzStr match {
      case "Int" => classOf[Int]
      case "String" => classOf[String]
      case "Long" => classOf[Long]
      case "Float" => classOf[Float]
      case "Double" => classOf[Double]
      case "Boolean" => classOf[Boolean]
      case "Short" => classOf[Short]
      case "Byte" => classOf[Byte]
      case s if s.startsWith("Map") => Class.forName("scala.collection.immutable.Map")
      case s if s.startsWith("List") || s.startsWith("scala.List") => Class.forName("scala.collection.immutable.List")
      case s if s.startsWith("Set") => Class.forName("scala.collection.immutable.Set")
      case s if s.startsWith("Seq") || s.startsWith("scala.Seq") => Class.forName("scala.collection.immutable.Seq")
      case s if s.startsWith("Vector") || s.startsWith("scala.Vector") => Class.forName("scala.collection.immutable.Vector")
      case s if s.startsWith("Array") => Class.forName("scala.Array")
      //去泛型
      case s if s.endsWith("]") => Class.forName(s.substring(0, s.indexOf("[")))
      case s => Class.forName(s)
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

case class FieldAnnotationInfo(annotation: Any, fieldName: String)

case class methodAnnotationInfo(annotation: Any, method: MethodSymbol)

@scala.annotation.meta.field
class Ignore extends scala.annotation.StaticAnnotation
