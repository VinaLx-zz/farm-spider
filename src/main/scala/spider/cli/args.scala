package spider
package cli

import spider.spider3w3n._
import spider.Util.{ cwd, tryOption }
import spider.Util.DateTimeUtil.{ fromFormatString }
import spider.database.DBConfig

import slick.jdbc.JdbcBackend.Database

import com.github.nscala_time.time.Imports._

import scala.util.{ Try, Success, Failure }
import scala.io.Source

trait ArgMatcher[+A] {
  def unapply(args: Seq[String]): Option[(A, Seq[String])]
}

object ArgMatcher {
  def apply[A](
    pf: PartialFunction[Seq[String], (A, Seq[String])]) = new ArgMatcher[A] {
    def unapply(args: Seq[String]) = {
      val fallback: PartialFunction[Seq[String], Option[(A, Seq[String])]] = {
        case _ ⇒ None
      }
      (pf andThen (Some(_)) orElse fallback)(args)
    }
  }
}

trait Args {
  type Matcher[+A] = PartialFunction[Seq[String], (A, Seq[String])]
  def matcher: Matcher[Args]
  def update: ArgMatcher[Args]
}

case class UserArg(
  user: Option[String] = None,
  pass: Option[String] = None) extends Args {
  val matcher: Matcher[UserArg] = {
    case "--pass" +: pass +: tail ⇒ (copy(pass = Some(pass)), tail)
    case "--user" +: user +: tail ⇒ (copy(user = Some(user)), tail)
  }
  def update: ArgMatcher[UserArg] = ArgMatcher(matcher)

  def get: Option[User] = (user, pass) match {
    case (Some(u), Some(p)) ⇒ Some(User(u, p))
    case _ ⇒ None
  }
}

case class ConfigArg(configPath: String = cwd + "/db.json") extends Args {
  val matcher: Matcher[ConfigArg] = {
    case "--config" +: config +: tail ⇒ (copy(configPath = config), tail)
  }
  def update: ArgMatcher[ConfigArg] = ArgMatcher(matcher)

  def get: Try[DBConfig] = {
    Try(Source.fromFile(configPath).mkString("")) match {
      case Success(file) ⇒ DBConfig.loadJson(file)
      case Failure(e) ⇒ Failure(e)
    }
  }
}

case class DatesArg(
  from: Option[String] = None,
  to: Option[String] = None,
  dates: IndexedSeq[String] = IndexedSeq.empty[String])
  extends Args {
  private val validDateFormats = Seq(
    "yyyy-MM-dd", "yyyy-M-dd", "yyyy-M-d", "yyyy-MM-d")
  private def toDate(s: String): Try[DateTime] = {
    validDateFormats.toStream
      .map(fmt ⇒ Try(fromFormatString(s, fmt)))
      .find(_.isSuccess) match {
        case Some(t) ⇒ t
        case None ⇒ Failure(new Throwable(s"invalid date format '$s'"))
      }
  }
  private def toDates(ds: IndexedSeq[String]): Try[IndexedSeq[DateTime]] = {
    (ds map (toDate(_)))
      .foldLeft(Success(IndexedSeq.empty): Try[IndexedSeq[DateTime]]) {
        (acc, d) ⇒
          for {
            dates ← acc
            date ← d
          } yield dates :+ date
      }
  }
  val matcher: Matcher[DatesArg] = {
    case "--from" +: f +: tail ⇒ (copy(from = Some(f)), tail)
    case "--to" +: t +: tail ⇒ (copy(to = Some(t)), tail)
    case date +: tail if !date.startsWith("--") ⇒
      (copy(dates = dates :+ date), tail)
  }
  def update: ArgMatcher[DatesArg] = ArgMatcher(matcher)

  def get: Try[IndexedSeq[DateTime]] = (from, to) match {
    case (Some(fromStr), Some(toStr)) ⇒
      for {
        f ← toDate(fromStr)
        t ← toDate(toStr)
        ds ← toDates(dates)
      } yield (f.to(t).by[IndexedSeq](1.day)) ++ ds
    case _ ⇒ toDates(dates)
  }
}