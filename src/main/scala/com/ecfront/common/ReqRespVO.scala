package com.ecfront.common

import com.typesafe.scalalogging.slf4j.LazyLogging

import scala.concurrent.Promise
import scala.language.implicitConversions

/**
  * Request VO
  */
case class Req(token: String, login_Id: String, login_name: String, organization_id: String, organization_name: String, role_ids: Map[String, String])

object Req {
  val SYS_ADMIN_ID = "0"
  val ANONYMOUS_ID = "-1"
  val sysReq = Req("", SYS_ADMIN_ID, "system", "", "", null)
  val anonymousReq = Req("", ANONYMOUS_ID, "anonymous", "", "", null)
}

/**
  * Response VO
  *
  * @param code    Standard Code
  * @param message Description
  * @param _body   Response main info
  */
case class Resp[E](code: String, message: String, private val _body: Option[E] = null) {
  var body: E = _
}

object Resp extends LazyLogging {

  val CODE = "code"
  val BODY = "body"
  val MESSAGE = "message"
  val CUSTOM_CODE_PREFIX = "custom-"

  def success[E](body: E) = {
    val res = Resp[E](StandardCode.SUCCESS, "", Some(body))
    res.body = body
    res
  }

  def notFound[E](message: String) = {
    logger.warn("[Result] [%s] Not found: %s".format(StandardCode.NOT_FOUND, message))
    Resp[E](StandardCode.NOT_FOUND, message, null)
  }

  def badRequest[E](message: String) = {
    logger.warn("[Result] [%s] Bad request: %s".format(StandardCode.BAD_REQUEST, message))
    Resp[E](StandardCode.BAD_REQUEST, message, null)
  }

  def forbidden[E](message: String) = {
    logger.warn("[Result] [%s] Forbidden: %s".format(StandardCode.FORBIDDEN, message))
    Resp[E](StandardCode.FORBIDDEN, message, null)
  }

  def unAuthorized[E](message: String) = {
    logger.warn("[Result] [%s] Unauthorized: %s".format(StandardCode.UNAUTHORIZED, message))
    Resp[E](StandardCode.UNAUTHORIZED, message, null)
  }

  def serverError[E](message: String) = {
    logger.error("[Result] [%s] Server error: %s".format(StandardCode.INTERNAL_SERVER_ERROR, message))
    Resp[E](StandardCode.INTERNAL_SERVER_ERROR, message, null)
  }

  def notImplemented[E](message: String) = {
    logger.error("[Result] [%s] Not implemented: %s".format(StandardCode.NOT_IMPLEMENTED, message))
    Resp[E](StandardCode.NOT_IMPLEMENTED, message, null)
  }

  def serverUnavailable[E](message: String) = {
    logger.error("[Result] [%s] Server unavailable: %s".format(StandardCode.SERVICE_UNAVAILABLE, message))
    Resp[E](StandardCode.SERVICE_UNAVAILABLE, message, null)
  }

  def unknown[E](message: String) = {
    logger.error("[Result] [%s] unknown fail: %s".format(StandardCode.UNKNOWN, message))
    Resp[E](StandardCode.UNKNOWN, message, null)
  }

  def customFail[E](code: String, message: String) = {
    logger.error("[Result] [%s] Custom fail: %s".format(CUSTOM_CODE_PREFIX + code, message))
    Resp[E](CUSTOM_CODE_PREFIX + code, message, null)
  }

  implicit def isSuccess[E](dto: Resp[E]): Boolean = StandardCode.SUCCESS == dto.code

  implicit def convertFail[E](dto: Resp[_]): Resp[E] = Resp[E](dto.code, dto.message, null)

}

/**
  * Async responseVO
  *
  * @param p Promise
  */
case class AsyncResp[E](p: Promise[Resp[E]]) extends LazyLogging {

  def resp(dto: Resp[_]) = {
    val resp = Resp[E](dto.code, dto.message, Some(dto.body.asInstanceOf[E]))
    resp.body = dto.body.asInstanceOf[E]
    p.success(resp)
  }

  def success(body: E) = {
    p.success(Resp.success[E](body))
  }

  def notFound(message: String) = {
    p.success(Resp.notFound(message))
  }

  def badRequest(message: String) = {
    p.success(Resp.badRequest(message))
  }

  def forbidden(message: String) = {
    p.success(Resp.forbidden(message))
  }

  def unAuthorized(message: String) = {
    p.success(Resp.unAuthorized(message))
  }

  def serverError(message: String) = {
    p.success(Resp.serverError(message))
  }

  def notImplemented(message: String) = {
    p.success(Resp.notImplemented(message))
  }

  def serverUnavailable(message: String) = {
    p.success(Resp.serverUnavailable(message))
  }

  def unknown(message: String) = {
    p.success(Resp.unknown(message))
  }

  def customFail(code: String, message: String) = {
    p.success(Resp.customFail(code, message))
  }

}

/**
  * Standard Code
  */
object StandardCode extends Enumeration {
  val SUCCESS = Value("200").toString
  val BAD_REQUEST = Value("400").toString
  val UNAUTHORIZED = Value("401").toString
  val FORBIDDEN = Value("403").toString
  val NOT_FOUND = Value("404").toString
  val INTERNAL_SERVER_ERROR = Value("500").toString
  val NOT_IMPLEMENTED = Value("501").toString
  val SERVICE_UNAVAILABLE = Value("503").toString
  val UNKNOWN = Value("-1").toString
}
