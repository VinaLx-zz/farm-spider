package spider
package log

import ch.qos.logback.classic.{ Level, Logger ⇒ LogbackLogger }
import ch.qos.logback.classic.spi.{ ILoggingEvent ⇒ Event }
import ch.qos.logback.core.FileAppender

import org.slf4j.Logger.ROOT_LOGGER_NAME
import org.slf4j.LoggerFactory

object Log {
  lazy val root: LogbackLogger = logger(ROOT_LOGGER_NAME)

  def logger(name: String): LogbackLogger = {
    LoggerFactory.getLogger(name).asInstanceOf[LogbackLogger]
  }
  def logger[A](c: Class[A]): LogbackLogger = {
    LoggerFactory.getLogger(c).asInstanceOf[LogbackLogger]
  }

  def fileAppender(file: String): FileAppender[Event] = {
    val ret = new FileAppender[Event]()
    ret.openFile(file)
    ret
  }

  // implicit class Logback
}