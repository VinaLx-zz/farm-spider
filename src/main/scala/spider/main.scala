package spider

import spider.cli._
import ch.qos.logback.classic.Level
import org.slf4j.LoggerFactory

object Main {
  def setRootLoggerWarning(): Unit = {
    val logger = LoggerFactory
      .getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME)
      .asInstanceOf[ch.qos.logback.classic.Logger]
    logger.setLevel(Level.WARN)
  }

  def main(args: Array[String]): Unit = {
    setRootLoggerWarning()
    CLI.go(args)
  }
}