package spider

import spider.Util.DateTimeUtil.{ fromFormatString, toFormatString }
import spider.Util.{ tryOption, cwd }
import spider.database.{ DBConfig, FarmDB }
import spider.spider3w3n._

import com.github.nscala_time.time.Imports._
import java.nio.file.Path
import scala.util.{ Try, Success, Failure }
import scala.io.Source

object CLI {
  case class UniversalArgs(
    val username: Option[String] = None,
    val password: Option[String] = None,
    val confPath: String = cwd + "/db.json")

  object Scrape {
    case class ScrapeArgs(
      from: Option[DateTime] = None,
      to: Option[DateTime] = None,
      dates: IndexedSeq[DateTime] = IndexedSeq.empty,
      u: UniversalArgs = UniversalArgs(),
      parallelism: Int = 1)

    implicit class StringConvertOption(s: String) {
      val validDateFormats = Seq(
        "yyyy-MM-dd", "yyyy-M-dd", "yyyy-M-d", "yyyy-MM-d")
      def toDateOption: Option[DateTime] = {
        validDateFormats.toStream
          .map { format ⇒ tryOption(fromFormatString(s, format)) }
          .find(!_.isEmpty).flatten
      }
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
        case "--from" +: from +: tail ⇒ from.toDateOption match {
          case Some(d) ⇒ parseArgsImpl(acc.copy(from = Some(d)), tail)
          case None ⇒ errorExit(s"invalid date format: $from")
        }
        case "--to" +: to +: tail ⇒ to.toDateOption match {
          case Some(d) ⇒ parseArgsImpl(acc.copy(to = Some(d)), tail)
          case None ⇒ errorExit(s"invalid date format: $to")
        }
        case "--parallelism" +: i +: tail ⇒ i.toIntOption match {
          case Some(i) ⇒ parseArgsImpl(acc.copy(parallelism = i), tail)
          case None ⇒ errorExit(s"parallelism must be an integer")
        }
        case "--config" +: p +: tail ⇒
          parseArgsImpl(acc.copy(u = acc.u.copy(confPath = p)), tail)
        case "--user" +: user +: tail ⇒
          parseArgsImpl(acc.copy(u = acc.u.copy(username = Some(user))), tail)
        case "--pass" +: pass +: tail ⇒
          parseArgsImpl(acc.copy(u = acc.u.copy(password = Some(pass))), tail)
        case date +: tail ⇒ date.toDateOption match {
          case Some(d) ⇒ parseArgsImpl(acc.copy(dates = acc.dates :+ d), tail)
          case None ⇒ errorExit(s"invalid date format: $date")
        }
      }
      parseArgsImpl(ScrapeArgs(), args)
    }

    def extractDates(args: ScrapeArgs): IndexedSeq[DateTime] = {
      (args.from, args.to) match {
        case (Some(from), Some(to)) ⇒
          args.dates ++ from.to(to).by[IndexedSeq](1.day)
        case _ ⇒ args.dates
      }
    }

    def apply(args: Seq[String]): Unit = {
      val a = parseArgs(args)
      val dates = extractDates(a)
      val db = FarmDB.getConnection(getConfig(a.u.confPath))
      val user = extractUser(a.u)
      Runner.go(user, dates, Sinker.writeToDB(db), a.parallelism)
    }
  }
  object Help {
    def apply(args: Seq[String]): Unit = {
      println("this is a help message")
    }
  }
  object Wait {
    import slick.jdbc.JdbcBackend.Database

    case class ServeArgs(u: UniversalArgs = UniversalArgs())

    case class TimeOfDay(hour: Int, minute: Int, second: Int)

    val wakeUp = TimeOfDay(14, 45, 0)

    private def parseArgs(args: Seq[String]): ServeArgs = {
      @annotation.tailrec
      def parseArgsImpl(
        acc: ServeArgs, cur: Seq[String]): ServeArgs = cur match {
        case e if cur.isEmpty ⇒ acc
        case "--config" +: p +: tail ⇒
          parseArgsImpl(acc.copy(u = acc.u.copy(confPath = p)), tail)
        case "--user" +: user +: tail ⇒
          parseArgsImpl(acc.copy(u = acc.u.copy(username = Some(user))), tail)
        case "--pass" +: pass +: tail ⇒
          parseArgsImpl(acc.copy(u = acc.u.copy(password = Some(pass))), tail)
      }
      parseArgsImpl(ServeArgs(), args)
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
      val db = FarmDB.getConnection(getConfig(a.u.confPath))
      val user = extractUser(a.u)
      wait(db, user)
    }
  }

  def errorExit(msg: String, code: Int = 1): Nothing = {
    System.err.println("error: " + msg)
    sys.exit(1)
  }

  def getConfig(path: String): DBConfig = {
    Try(Source.fromFile(path).mkString("")) match {
      case Success(file) ⇒ DBConfig.loadJson(file) match {
        case Some(config) ⇒ config
        case None ⇒ errorExit("invalid config file format")
      }
      case Failure(e) ⇒ errorExit(e.getMessage)
    }
  }

  def extractUser(u: UniversalArgs): User = (u.username, u.password) match {
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