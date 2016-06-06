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
case class Resp[E](code: String, var message: String, private val _body: Option[E] = null) {
  var body: E = _

  Resp.customInit(this)
}

object Resp extends LazyLogging {

  // 自定义初始化方法
  var customInit: Resp[_] => Unit = { resp => }

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
    Resp[E](StandardCode.NOT_FOUND, message, null)
  }

  def conflict[E](message: String) = {
    Resp[E](StandardCode.CONFLICT, message, null)
  }

  def locked[E](message: String) = {
    Resp[E](StandardCode.LOCKED, message, null)
  }

  def unsupportedMediaType[E](message: String) = {
    Resp[E](StandardCode.UNSUPPORTED_MEDIA_TYPE, message, null)
  }

  def badRequest[E](message: String) = {
    Resp[E](StandardCode.BAD_REQUEST, message, null)
  }

  def forbidden[E](message: String) = {
    Resp[E](StandardCode.FORBIDDEN, message, null)
  }

  def unAuthorized[E](message: String) = {
    Resp[E](StandardCode.UNAUTHORIZED, message, null)
  }

  def serverError[E](message: String) = {
    Resp[E](StandardCode.INTERNAL_SERVER_ERROR, message, null)
  }

  def notImplemented[E](message: String) = {
    Resp[E](StandardCode.NOT_IMPLEMENTED, message, null)
  }

  def serverUnavailable[E](message: String) = {
    Resp[E](StandardCode.SERVICE_UNAVAILABLE, message, null)
  }

  def unknown[E](message: String) = {
    Resp[E](StandardCode.UNKNOWN, message, null)
  }

  def customFail[E](code: String, message: String) = {
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

  def conflict(message: String) = {
    p.success(Resp.conflict(message))
  }

  def locked(message: String) = {
    p.success(Resp.locked(message))
  }

  def unsupportedMediaType(message: String) = {
    p.success(Resp.unsupportedMediaType(message))
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
  val CONFLICT = Value("409").toString
  val LOCKED = Value("423").toString
  val UNSUPPORTED_MEDIA_TYPE = Value("415").toString
  val INTERNAL_SERVER_ERROR = Value("500").toString
  val NOT_IMPLEMENTED = Value("501").toString
  val SERVICE_UNAVAILABLE = Value("503").toString
  val UNKNOWN = Value("-1").toString
}
