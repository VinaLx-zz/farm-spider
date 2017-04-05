package spider.cli
package command

import spider.cli.Args.ConfigArg
import spider.spider3w3n.Runner
import spider.Util.errorExit
import spider.database.{ FarmDB, DBConfig }

import slick.jdbc.JdbcBackend.Database

object GetCategory {
  case class GetCategoryArgs(config: Args[DBConfig] = ConfigArg())
  private def parseArgs(args: Seq[String]): GetCategoryArgs = {
    @annotation.tailrec
    def parseArgsImpl(
      acc: GetCategoryArgs, cur: Seq[String]): GetCategoryArgs = cur match {
      case e if cur.isEmpty ⇒ acc
      case acc.config.update(config, next) ⇒
        parseArgsImpl(acc.copy(config = config), next)
      case other +: tail ⇒
        errorExit(s"unknown option $other")
    }
    parseArgsImpl(GetCategoryArgs(), args)
  }
  def apply(args: Seq[String]): Unit = {
    val a = parseArgs(args)
    val db = FarmDB.getConnection(a.config.getOrExit)
    Runner.getCategories(db)
  }
}