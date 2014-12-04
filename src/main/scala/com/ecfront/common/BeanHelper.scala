package com.ecfront.common

import org.apache.commons.beanutils.BeanUtilsBean

/**
 * Created by sunisle on 2014/9/16.
 */
object BeanHelper {
  private val copyPropertiesAdapter = new NullAwareBeanUtilsBean

  def copyProperties(dest: AnyRef, orig:AnyRef)= copyPropertiesAdapter.copyProperties(dest,orig)

}

class NullAwareBeanUtilsBean extends BeanUtilsBean {
  override def copyProperty(bean: scala.Any, name: String, value: scala.Any): Unit = {
    if (null != value) {
      super.copyProperty(bean, name, value)
    }
  }
}
