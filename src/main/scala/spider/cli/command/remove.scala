package spider.cli
package command

import spider.Util.{ errorExit }
import spider.cli.CLI._
import spider.cli.Args._
import spider.database._
import spider.database.FarmDB.SyncDB

import com.github.nscala_time.time.Imports._
import slick.jdbc.JdbcBackend.Database

object Remove {
  case class RemoveArgs(
    dates: Args[IndexedSeq[DateTime]] = DatesArg(),
    config: Args[DBConfig] = ConfigArg())

  def parseArgs(args: Seq[String]): RemoveArgs = {
    @annotation.tailrec
    def parseArgsImpl(
      acc: RemoveArgs, rest: Seq[String]): RemoveArgs = rest match {
      case e if rest.isEmpty ⇒ acc
      case acc.dates.update(dates, next) ⇒
        parseArgsImpl(acc.copy(dates = dates), next)
      case acc.config.update(config, next) ⇒
        parseArgsImpl(acc.copy(config = config), next)
      case other +: tail ⇒ errorExit(s"unknown option $other")
    }
    parseArgsImpl(RemoveArgs(), args)
  }
  def remove(db: Database, dates: Seq[DateTime]) = {
    import scala.concurrent.ExecutionContext.Implicits._
    db.runSync(FarmTable.clearRecordsAction(dates))
  }

  def apply(args: Seq[String]): Unit = {
    val a = parseArgs(args)
    val dates = a.dates.getOrExit
    val db = FarmDB.getConnection(a.config.getOrExit)
    remove(db, dates)
  }
}