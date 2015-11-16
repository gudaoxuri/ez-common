package com.ecfront.common

import com.typesafe.scalalogging.slf4j.LazyLogging

/**
  * 异步处理辅助类
  */
object AsyncHelper extends LazyLogging {

  /**
    * 异步处理结果封装，加入异常处理
    * @param fun 异步处理方法
    * @param resp 结果返回封装
    */
  def resp[E](fun: => Unit => E)(implicit resp: AsyncResp[E]) {
    try {
      resp.success(fun())
    } catch {
      case ex: Throwable =>
        logger.error(s"Async process error : ${ex.getMessage}", ex)
        resp.serverError(s"Async process error : ${ex.getMessage}")
    }
  }

}
