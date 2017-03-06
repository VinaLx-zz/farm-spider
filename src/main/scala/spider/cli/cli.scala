package spider
package cli

import spider.Util.DateTimeUtil.{ fromFormatString, toFormatString }
import spider.Util.{ tryOption, cwd }
import spider.database.{ DBConfig, FarmDB }
import spider.spider3w3n._

import com.github.nscala_time.time.Imports._
import java.nio.file.Path
import scala.util.{ Try, Success, Failure }
import scala.io.Source

object CLI {

  object Scrape {
    case class ScrapeArgs(
      dates: DatesArg = DatesArg(),
      config: ConfigArg = ConfigArg(),
      user: UserArg = UserArg(),
      parallelism: Int = 1)

    implicit class StringConvertOption(s: String) {
      def toIntOption: Option[Int] = Try(s.toInt) match {
        case Success(i) ⇒ Some(i)
        case Failure(e) ⇒ None
      }
    }

    private def parseArgs(args: Seq[String]): ScrapeArgs = {
      @annotation.tailrec
      def parseArgsImpl(
        acc: ScrapeArgs, cur: Seq[String]): ScrapeArgs = cur match {
        case e if cur.isEmpty ⇒ acc
        case acc.dates.update(dates, next) ⇒
          parseArgsImpl(acc.copy(dates = dates), next)
        case "--parallel" +: i +: tail ⇒ i.toIntOption match {
          case Some(i) ⇒ parseArgsImpl(acc.copy(parallelism = i), tail)
          case None ⇒ errorExit(s"parallelism must be an integer")
        }
        case acc.config.update(config, next) ⇒
          parseArgsImpl(acc.copy(config = config), next)
        case acc.user.update(user, next) ⇒
          parseArgsImpl(acc.copy(user = user), next)
        case "--parallelism" +: p +: tail ⇒ p.toIntOption match {
          case None ⇒ errorExit("parallelism must be an integer")
          case Some(i) ⇒ parseArgsImpl(acc.copy(parallelism = i), tail)
        }
        case other +: tail ⇒
          errorExit(s"unknown option $other")
      }
      parseArgsImpl(ScrapeArgs(), args)
    }

    def extractDates(args: ScrapeArgs): IndexedSeq[DateTime] =
      args.dates.get match {
        case Success(dates) ⇒ dates
        case Failure(e) ⇒ errorExit(e.getMessage)
      }

    def apply(args: Seq[String]): Unit = {
      val a = parseArgs(args)
      val dates = extractDates(a)
      val db = FarmDB.getConnection(getConfig(a.config))
      val user = extractUser(a.user)
      println(a)
      // Runner.go(user, dates, Sinker.writeToDB(db), a.parallelism)
    }
  }
  object Help {
    def apply(args: Seq[String]): Unit = {
      println("this is a help message")
    }
  }
  object Wait {
    import slick.jdbc.JdbcBackend.Database

    case class WaitArgs(
      user: UserArg = UserArg(),
      config: ConfigArg = ConfigArg())

    case class TimeOfDay(hour: Int, minute: Int, second: Int)

    val wakeUp = TimeOfDay(14, 45, 0)

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
        println(
          s"sleep until ${toFormatString(wakeUpTime, "yyyy-MM-dd HH:mm:ss")}")
        sleepUntil(wakeUpTime)
        println(s"start scraping records of ${toFormatString(wakeUpTime)}")
        Runner.go(user = user, sink = Sinker.writeToDB(db))
        println("put to sleep")
      }
    }

    def apply(args: Seq[String]): Unit = {
      val a = parseArgs(args)
      val db = FarmDB.getConnection(getConfig(a.config))
      val user = extractUser(a.user)
      wait(db, user)
    }
  }

  def errorExit(msg: String, code: Int = 1): Nothing = {
    System.err.println("error: " + msg)
    sys.exit(1)
  }

  def getConfig(arg: ConfigArg): DBConfig = arg.get match {
    case Success(config) ⇒ config
    case Failure(e) ⇒ errorExit(e.getMessage)
  }

  def extractUser(u: UserArg): User = (u.user, u.pass) match {
    case (Some(user), Some(pass)) ⇒ User(user, pass)
    case _ ⇒ errorExit("require username and password for 3w3n.com")
  }

  def main(args: Array[String]): Unit = args.toSeq match {
    case e if args.isEmpty ⇒ Help(args)
    case "scrape" +: tail ⇒ Scrape(tail)
    case "wait" +: tail ⇒ Wait(tail)
    case "help" +: tail ⇒ Help(tail)
    case other +: tail ⇒ errorExit(s"unknown command $other")
  }
}