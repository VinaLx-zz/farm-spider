package spider.cli
package command

import spider.Util.{ errorExit }
import spider.Util.DateTimeUtil.toFormatString
import spider.cli.Args._
import spider.cli.CLI._
import spider.database._
import spider.spider3w3n.{ Runner, Sinker, User }

import com.github.nscala_time.time.Imports._

object Wait {
  import slick.jdbc.JdbcBackend.Database

  case class WaitArgs(
    user: Args[User] = UserArg(),
    config: Args[DBConfig] = ConfigArg())

  case class TimeOfDay(hour: Int, minute: Int, second: Int)

  val wakeUp = TimeOfDay(12, 0, 0)

  private def parseArgs(args: Seq[String]): WaitArgs = {
    @annotation.tailrec
    def parseArgsImpl(
      acc: WaitArgs, cur: Seq[String]): WaitArgs = cur match {
      case e if cur.isEmpty ⇒ acc
      case acc.config.update(config, next) ⇒
        parseArgsImpl(acc.copy(config = config), next)
      case acc.user.update(user, next) ⇒
        parseArgsImpl(acc.copy(user = user), next)
      case other +: tail ⇒
        errorExit(s"unknown option $other")
    }
    parseArgsImpl(WaitArgs(), args)
  }

  def nextWakeUp(from: DateTime): DateTime = {
    val todayWakeUp =
      from.hour(wakeUp.hour).minute(wakeUp.minute).second(wakeUp.second)
    if (from > todayWakeUp) todayWakeUp + 1.day
    else todayWakeUp
  }

  def sleepUntil(to: DateTime): Unit = {
    Thread.sleep(to.getMillis - DateTime.now.getMillis)
  }

  private def wait(db: Database, user: User): Unit = {
    while (true) {
      // there is a veeeerrrryyyy short time elapse between the two function
      // call, but it should be ok in 99.999% of cases
      val wakeUpTime = nextWakeUp(DateTime.now)
      logger.info(
        s"sleep until ${toFormatString(wakeUpTime, "yyyy-MM-dd HH:mm:ss")}")
      sleepUntil(wakeUpTime)
      logger.info(s"start scraping records of ${toFormatString(wakeUpTime)}")
      Runner.go(user = user, sink = Sinker.writeToDB(db))
      logger.info("put to sleep")
    }
  }

  def apply(args: Seq[String]): Unit = {
    val a = parseArgs(args)
    val db = FarmDB.getConnection(a.config.getOrExit)
    val user = a.user.getOrExit
    wait(db, user)
  }
}