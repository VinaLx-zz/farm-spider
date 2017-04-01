package spider.cli
package command

import spider.Util.{ errorExit }
import spider.spider3w3n.{ User, Runner, Sinker }
import spider.cli.CLI._
import spider.cli.Args._
import spider.database.{ DBConfig, FarmDB }

import com.github.nscala_time.time.Imports._
import slick.jdbc.JdbcBackend.Database

import scala.util.{ Try, Success, Failure }

object Scrape {
  case class ScrapeArgs(
    dates: Args[IndexedSeq[DateTime]] = DatesArg(),
    config: Args[DBConfig] = ConfigArg(),
    user: Args[User] = UserArg(),
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
      case other +: tail ⇒
        errorExit(s"unknown option $other")
    }
    parseArgsImpl(ScrapeArgs(), args)
  }

  def scrape(
    db: Database, user: User, dates: IndexedSeq[DateTime], parallel: Int) =
    Runner.go(user, dates, Sinker.writeToDB(db), parallel)

  def apply(args: Seq[String]): Unit = {
    val a = parseArgs(args)
    val dates = a.dates.getOrExit
    val db = FarmDB.getConnection(a.config.getOrExit)
    val user = a.user.getOrExit
    logger.info(a.toString)
    scrape(db, user, dates, a.parallelism)
  }
}