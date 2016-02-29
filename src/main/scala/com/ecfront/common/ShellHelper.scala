package com.ecfront.common

import java.io.{BufferedReader, IOException, InputStream, InputStreamReader}
import java.util.concurrent.{CountDownLatch, Executors}

import com.typesafe.scalalogging.slf4j.LazyLogging

import scala.concurrent.{Future, Promise}

/**
  * Shell辅助类
  */
case class ShellHelper(command: String, reportHandler: ReportHandler = null, taskId: String = "", successFlag: String = null, progressFlag: String = null) extends LazyLogging {

  private val result = new StringBuffer
  private var returnResult = false

  def execute(rr: Boolean): Unit = {
    returnResult = rr
    try {
      val process = Runtime.getRuntime.exec(Array[String]("/bin/sh", "-c", command.replaceAll("\r\n\\s*", " ").replaceAll("\n\\s*", " ")))
      if (process != null) {
        val errorGobbler = new StreamGobbler(process.getErrorStream)
        val outputGobbler = new StreamGobbler(process.getInputStream)
        val errorFuture = Executors.newCachedThreadPool.submit(errorGobbler)
        val outputFuture = Executors.newCachedThreadPool.submit(outputGobbler)
        if (0 == process.waitFor) {
          val res1 = errorFuture.get
          val res2 = outputFuture.get
          if (reportHandler != null) {
            //删除最后一行（\r\n）
            reportHandler.complete(taskId, if (result.length > 0) result.substring(0, result.length - 2) else result.toString)
          }
          logger.debug("Execute Complete: " + command)
          if (!res1 && !res2) {
            if (reportHandler != null) {
              reportHandler.fail(taskId, "Not Find successFlag [" + successFlag + "], shellPath:" + command)
            }
            logger.warn("Execute fail: Not Find successFlag [" + successFlag + "], shellPath:" + command)
          }
        } else {
          logger.warn("Execute fail: Abnormal termination , shellPath:" + command)
          if (reportHandler != null) {
            reportHandler.fail(taskId, "Abnormal termination , shellPath:" + command)
          }
        }
      } else {
        logger.warn("Execute fail: PID NOT exist , shellPath:" + command)
        if (reportHandler != null) {
          reportHandler.fail(taskId, "PID NOT exist , shellPath:" + command)
        }
      }
    } catch {
      case e: Exception =>
        logger.error("Execute fail: ", e)
        if (reportHandler != null) {
          reportHandler.fail(taskId, e.getMessage + " , shellPath:" + command)
        }
    }
  }

  /**
    * 输出处理
    */
  private class StreamGobbler(is: InputStream) extends java.util.concurrent.Callable[Boolean] {

    def call: Boolean = {
      var isr: InputStreamReader = null
      var br: BufferedReader = null
      var line: String = null
      var success: Boolean = false
      try {
        isr = new InputStreamReader(is)
        br = new BufferedReader(isr)
        line = br.readLine
        while (line != null) {
          logger.trace("Shell content:" + line)
          if (returnResult) {
            result.append(line + "\r\n")
          }
          if (successFlag != null && line.contains(successFlag)) {
            if (reportHandler != null) {
              reportHandler.success(taskId)
            }
            success = true
          }
          if (progressFlag != null && reportHandler != null && line.contains(progressFlag)) {
            reportHandler.progress(taskId, Integer.valueOf(line.substring(line.indexOf(progressFlag) + progressFlag.length).trim))
          }
          line = br.readLine
        }
        if (successFlag == null) {
          true
        } else {
          success
        }
      } catch {
        case e: IOException =>
          logger.warn("Execute fail: ", e)
          false
      } finally {
        if (br != null) {
          try {
            br.close()
          }
          catch {
            case e: Exception =>
              logger.warn("Execute warn: ", e)
          }
        }
        if (isr != null) {
          try {
            isr.close()
          }
          catch {
            case e: Exception =>
              logger.warn("Execute warn: ", e)
          }
        }
        if (is != null) {
          try {
            is.close()
          }
          catch {
            case e: Exception =>
              logger.warn("Execute warn: ", e)
          }
        }
      }
    }
  }

}

object ShellHelper {

  /**
    * @param command       shell命令
    * @param reportHandler 任务报告实例
    * @param taskId        任务ID
    * @param successFlag   成功标识，只要捕捉到此标识就视为成功
    * @param progressFlag  进度标识，只要捕捉到此标识就更新进度， 格式为 <progressFlag>空格<progress>,如： progress 40
    * @param returnResult  是否返回结果
    */
  def async(command: String, reportHandler: ReportHandler, taskId: String, successFlag: String, progressFlag: String, returnResult: Boolean): Unit = {
    new ShellHelper(command, reportHandler, taskId, successFlag, progressFlag).execute(returnResult)
  }

  def async(command: String, reportHandler: ReportHandler, taskId: String, returnResult: Boolean): Unit = {
    new ShellHelper(command, reportHandler, taskId).execute(returnResult)
  }

  def async(command: String, reportHandler: ReportHandler, returnResult: Boolean): Unit = {
    new ShellHelper(command, reportHandler).execute(returnResult)
  }

  def async(command: String): Unit = {
    new ShellHelper(command).execute(rr = false)
  }

  def async(command: String, successFlag: String, returnResult: Boolean): Future[Resp[String]] = {
    val p = Promise[Resp[String]]()
    var isSuccess = false
    var resp: Resp[String] = null
    new ShellHelper(command, new ReportHandler {

      override def progress(taskId: String, progress: Int): Unit = {}

      override def success(taskId: String): Unit = {
        isSuccess = true
      }

      override def complete(taskId: String, result: String): Unit = {
        if (successFlag != null && !isSuccess) {
          p.success(Resp.serverError("Success Flag NOT found"))
        } else if (resp == null) {
          p.success(Resp.success(result))
        } else {
          p.success(resp)
        }
      }

      override def fail(taskId: String, message: String): Unit = {
        resp = Resp.serverError(message)
      }
    }, System.nanoTime() + "", successFlag).execute(returnResult)
    p.future
  }

  def sync(command: String, successFlag: String, returnResult: Boolean): Resp[String] = {
    val completeFlag = new CountDownLatch(1)
    var isSuccess = false
    var resp: Resp[String] = null
    new ShellHelper(command, new ReportHandler {

      override def progress(taskId: String, progress: Int): Unit = {}

      override def success(taskId: String): Unit = {
        isSuccess = true
      }

      override def complete(taskId: String, result: String): Unit = {
        if (successFlag != null && !isSuccess) {
          resp = Resp.serverError("Success Flag NOT found")
        } else if (resp == null) {
          resp = Resp.success(result)
        }
        completeFlag.countDown()
      }

      override def fail(taskId: String, message: String): Unit = {
        resp = Resp.serverError(message)
      }
    }, System.nanoTime() + "", successFlag).execute(returnResult)
    completeFlag.await()
    resp
  }

  def sync(command: String, returnResult: Boolean = false): String = {
    val completeFlag = new CountDownLatch(1)
    var res: String = ""
    new ShellHelper(command, new ReportHandler {
      override def progress(taskId: String, progress: Int): Unit = {}

      override def success(taskId: String): Unit = {
        completeFlag.countDown()
      }

      override def complete(taskId: String, result: String): Unit = {
        res = result
        completeFlag.countDown()
      }

      override def fail(taskId: String, message: String): Unit = {
        completeFlag.countDown()
      }
    }).execute(returnResult)
    completeFlag.await()
    res
  }

}

/**
  * 任务报告接口
  */
trait ReportHandler {
  /**
    * 成功
    *
    * @param taskId 任务ID
    */
  def success(taskId: String)

  /**
    * 失败
    *
    * @param taskId  任务ID
    * @param message 失败原因描述
    */
  def fail(taskId: String, message: String)

  /**
    * 进度
    *
    * @param taskId   任务ID
    * @param progress 0-100
    */
  def progress(taskId: String, progress: Int)

  /**
    * 完成
    *
    * @param taskId 任务ID
    * @param result 结果
    */
  def complete(taskId: String, result: String)

}
