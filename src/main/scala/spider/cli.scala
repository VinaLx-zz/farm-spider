package spider

import spider.Util.DateTimeUtil.fromFormatString
import spider.Util.{ tryOption, cwd }
import spider.database.DBConfig

import com.github.nscala_time.time.Imports._
import java.nio.file.Path
import scala.util.{ Try, Success, Failure }
import scala.io.Source

object CLI {
  object Scrape {
    case class ScrapeArgs(
      from: Option[DateTime] = None,
      to: Option[DateTime] = None,
      dates: IndexedSeq[DateTime] = IndexedSeq.empty,
      configPath: String = cwd + "db.json",
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

    def parseArgs(args: Seq[String]): ScrapeArgs = {
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
          parseArgsImpl(acc.copy(configPath = p), tail)
        case date +: tail ⇒ date.toDateOption match {
          case Some(d) ⇒ parseArgsImpl(acc.copy(dates = acc.dates :+ d), tail)
          case None ⇒ errorExit(s"invalid date format: $date")
        }
      }
      parseArgsImpl(ScrapeArgs(), args)
    }

    def apply(args: Seq[String]): Unit = {
      val ScrapeArgs(from, to, dates, confPath, parallelism) = parseArgs(args)
      val finalDates = (from, to) match {
        case (Some(from), Some(to)) ⇒ dates ++ from.to(to).by[IndexedSeq](1.day)
        case _ ⇒ dates
      }
      val dbconfig = getConfig(confPath)
    }
  }
  object Help {
    def apply(args: Seq[String]): Unit = {
      println("this is a help message")
    }
  }
  object Serve {
    def apply(args: Seq[String]): Unit = {
      println("serving...")
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

  def main(args: Array[String]): Unit = args.toSeq match {
    case e if args.isEmpty ⇒ Help(args)
    case "scrape" +: tail ⇒ Scrape(tail)
    case "serve" +: tail ⇒ Serve(tail)
    case "help" +: tail ⇒ Help(tail)
    case other +: tail ⇒ errorExit(s"unknown command $other")
  }
}