package com.ecfront.common

import java.io.{File, FileFilter}
import java.net.{JarURLConnection, URLDecoder}
import java.util.jar.JarFile

import scala.collection.JavaConversions._
import scala.collection.mutable.ArrayBuffer
import scala.reflect.runtime.universe._

/**
 * Class扫描类
 */
object ClassScanHelper {

  /**
   * 获取包含指定注解的class列表
   * @param basePackage 查找的根包名
   * @param ignoreInterface 是否忽略接口（包含trait和abstract）
   * @tparam A 指定注解类型
   * @return class列表
   */
  def scan[A: TypeTag](basePackage: String, ignoreInterface: Boolean = true): List[Class[_]] = {
    val packageDir = basePackage.replace('.', '/')
    Thread.currentThread().getContextClassLoader.getResources(packageDir).flatMap {
      url =>
        url.getProtocol match {
          case "file" => findAndAddClassesByFile(basePackage, new File(URLDecoder.decode(url.getFile, "UTF-8")), typeOf[A], ignoreInterface)
          case "jar" => findAndAddClassesByJar(url.openConnection().asInstanceOf[JarURLConnection].getJarFile, packageDir, typeOf[A], ignoreInterface)
          case _ => List[Class[_]]()
        }
    }.toList
  }

  private def findAndAddClassesByFile(currentPackage: String, currentFile: File, typeAnnotation: Type, ignoreInterface: Boolean): List[Class[_]] = {
    val result = ArrayBuffer[Class[_]]()
    if (currentFile.exists() && currentFile.isDirectory) {
      currentFile.listFiles(new FileFilter() {
        override def accept(file: File): Boolean = {
          file.isDirectory || file.getName.endsWith(".class")
        }
      }).foreach {
        file =>
          if (file.isDirectory) {
            result ++= findAndAddClassesByFile(currentPackage + "." + file.getName, file, typeAnnotation, ignoreInterface)
          } else {
            val className = file.getName.substring(0, file.getName.length() - 6)
            //Filter Inner Class
            if (!className.substring(0, className.length - 1).contains("$")) {
              val clazz = Thread.currentThread().getContextClassLoader.loadClass(currentPackage + '.' + className)
              if (isMatch(typeAnnotation, clazz, ignoreInterface)) {
                result += clazz
              }
            }
          }
      }
    }
    result.toList
  }

  private def findAndAddClassesByJar(jar: JarFile, currentPath: String, typeAnnotation: Type, ignoreInterface: Boolean): List[Class[_]] = {
    val result = ArrayBuffer[Class[_]]()
    jar.entries().foreach {
      jarEntry =>
        var jarName = jarEntry.getName
        if (jarName.charAt(0) == '/') {
          jarName = jarName.substring(1)
        }
        if (jarName.startsWith(currentPath)) {
          val idx = jarName.lastIndexOf('/')
          if (jarName.endsWith(".class") && !jarEntry.isDirectory) {
            val path = jarName.substring(0, idx)
            val className = jarName.substring(jarName.lastIndexOf('/') + 1, jarName.length() - 6)
            //Filter Inner Class
            if (!className.substring(0, className.length - 1).contains("$")) {
              val clazz = Class.forName(path.replace('/', '.') + '.' + className)
              if (isMatch(typeAnnotation, clazz, ignoreInterface)) {
                result += clazz
              }
            }
          }
        }
    }
    result.toList
  }

  private def isMatch(typeAnnotation: Type, clazz: Class[_], ignoreInterface: Boolean): Boolean = {
    val scalaType = scala.reflect.runtime.currentMirror.classSymbol(clazz).toType.typeSymbol
    (scalaType.isClass && !ignoreInterface || ignoreInterface && !scalaType.asClass.isTrait && !scalaType.asClass.isAbstract) && scalaType.asClass.annotations.exists(a => a.tree.tpe == typeAnnotation)
  }

}
