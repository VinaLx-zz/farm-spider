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

  val wakeUpTimes = Seq(TimeOfDay(12, 0, 0), TimeOfDay(21, 0, 0))

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

  private def nextWakeUp(from: DateTime, target: TimeOfDay): DateTime = {
    val todayWakeUp =
      from.hour(target.hour).minute(target.minute).second(target.second)
    if (from > todayWakeUp) todayWakeUp + 1.day
    else todayWakeUp
  }

  private def sleepUntil(to: DateTime): Unit = {
    logger.info(
      s"sleep until ${toFormatString(to, "yyyy-MM-dd HH:mm:ss")}")
    Thread.sleep(to.getMillis - DateTime.now.getMillis)
  }

  private def wakeUp(db: Database, user: User) = {
    val time = DateTime.now
    logger.info(s"start scraping records of ${toFormatString(time)}")
    Scrape.scrapeOne(db, user, time)
    logger.info(s"${toFormatString(time)} finish")
  }

  private def waitOne(db: Database, user: User, wakeUpTime: DateTime) = {
    sleepUntil(wakeUpTime)
    wakeUp(db, user)
  }

  private def wait(db: Database, user: User): Unit = {
    while (true) {
      for {
        timeOfDay ← wakeUpTimes
      } {
        val nextTime = nextWakeUp(DateTime.now, timeOfDay)
        waitOne(db, user, nextTime)
      }
    }
  }

  def apply(args: Seq[String]): Unit = {
    val a = parseArgs(args)
    val db = FarmDB.getConnection(a.config.getOrExit)
    val user = a.user.getOrExit
    wait(db, user)
  }
}