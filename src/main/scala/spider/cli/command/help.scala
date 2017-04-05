package spider.cli
package command

object Help {
  lazy val usage =
    """usage:
        |    run help [command]
        |    run wait option...
        |    run scrape option...
        |    run remove option...""".stripMargin
  lazy val waitUsage =
    """wait -- wait everyday product infomations and scrape at 12:00:00
        |run wait --user u --pass p [--config path]
        |    options:
        |        --user: username for 3w3n.com
        |        --pass: password for 3w3n.com
        |        --config: path to database configuration file, default to ./db.json
        |    note:
        |        for the format for database configuration, see example db.json
      """.stripMargin

  lazy val scrapeUsage =
    """scrape -- scrape product infomations at certain dates into database
        |run scrape --user u --pass p [--from date --to date] [--parallel p] [--config path] date...
        |    options:
        |        --user: username for 3w3n.com
        |        --pass: password for 3w3n.com
        |        --from, --to: the range of date of scraping
        |        --parallel: number of thread launched to scrape data, default 1
        |        --config: path to database configuration file, default to ./db.json
        |    note:
        |        allowed date format: yyyy-MM-dd, yyyy-M-dd, yyyy-MM-d, yyyy-M-d
        |
        |        for the format for database configuration, see example db.json
        |
        |        each thread scrapes data at least for an ENTIRE day,
        |        so if parallelism > number of dates, threads actually launched would be less than required
      """.stripMargin

  lazy val removeUsage =
    """remove -- remove data from database for certain dates
        |run remove [--from date --to date] [--config path] date...
        |    options:
        |        --from, --to: the range of date of scraping
        |        --config: path to database configuration file, default to ./db.json
        |    note:
        |        allowed date format: yyyy-MM-dd, yyyy-M-dd, yyyy-MM-d, yyyy-M-d
        |
        |        for the format for database configuration, see example db.json
      """.stripMargin

  lazy val getcatUsage =
    """getcat -- update categories of product in the database
       |run getcat [--config path]
       |    options:
       |        --config: path to database configuration file, default to ./db.json
       |    note:
       |        for the format for database configuration, see example db.json
    """.stripMargin
  def apply(args: Seq[String]): Unit = args match {
    case e if args.isEmpty ⇒ println(usage)
    case "wait" +: ignore ⇒ println(waitUsage)
    case "remove" +: ignore ⇒ println(removeUsage)
    case "scrape" +: ignore ⇒ println(scrapeUsage)
    case "getcat" +: ignore ⇒ println(getcatUsage)
    case other +: ignore ⇒ println(s"$usage\nunknown command $other")
  }
}