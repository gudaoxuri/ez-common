package com.ecfront.common

object FormatHelper {

  def validEmail(email: String): Boolean =
    """(?=[^\s]+)(?=(\w+)@([\w\.]+))""".r.findFirstIn(email).nonEmpty

}
