package spider.cli
package command

import spider.Util.{ errorExit }
import spider.cli.CLI._
import spider.cli.Args._
import spider.database._
import spider.database.FarmDB.SyncDB

import com.github.nscala_time.time.Imports._

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
  def apply(args: Seq[String]): Unit = {
    import scala.concurrent.ExecutionContext.Implicits._
    val a = parseArgs(args)
    val dates = a.dates.getOrExit
    val db = FarmDB.getConnection(a.config.getOrExit)
    db.runSync(FarmTable.clearRecordsAction(dates))
  }
}