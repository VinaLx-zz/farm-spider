package spider

import spider.cli._
import spider.log.Log
import ch.qos.logback.classic.Level

object Main {

  def main(args: Array[String]): Unit = {
    // Log.root.setLevel(Level.WARN)
    CLI.go(args)
  }
}