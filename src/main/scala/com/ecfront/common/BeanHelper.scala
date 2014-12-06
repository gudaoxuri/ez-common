package com.ecfront.common

import org.apache.commons.beanutils.BeanUtilsBean

/**
 * Bean操作辅助类
 */
object BeanHelper {
  private val copyPropertiesAdapter = new NullAwareBeanUtilsBean

  /**
   * Bean 复制，忽略Null值属性
   * @param dest 目录Bean
   * @param orig 源Bean
   */
  def copyProperties(dest: AnyRef, orig: AnyRef) = copyPropertiesAdapter.copyProperties(dest, orig)

}

private class NullAwareBeanUtilsBean extends BeanUtilsBean {
  override def copyProperty(bean: scala.Any, name: String, value: scala.Any): Unit = {
    if (null != value) {
      super.copyProperty(bean, name, value)
    }
  }
}
