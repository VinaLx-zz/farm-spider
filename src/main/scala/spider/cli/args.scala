package spider
package cli

import spider.spider3w3n._
import spider.Util.{ cwd, tryOption, errorExit }
import spider.Util.DateTimeUtil.{ fromFormatString }
import spider.database.DBConfig

import slick.jdbc.JdbcBackend.Database

import com.github.nscala_time.time.Imports._

import scala.util.{ Try, Success, Failure }
import scala.io.Source

import Args._

trait ArgMatcher[+A] {
  def unapply(args: Seq[String]): Option[(Args[A], Seq[String])]
}

object ArgMatcher {
  def apply[A](
    pf: PartialFunction[Seq[String], (Args[A], Seq[String])]) = new ArgMatcher[A] {
    def unapply(args: Seq[String]) = {
      val fallback: PartialFunction[Seq[String], Option[(Args[A], Seq[String])]] = {
        case _ ⇒ None
      }
      (pf andThen (Some(_)) orElse fallback)(args)
    }
  }
}

trait Args[+A] {
  def matcher: Matcher[A]
  def update: ArgMatcher[A] = ArgMatcher(matcher)
  def get: Try[A]
  def getOrExit: A = get match {
    case Success(value) ⇒ value
    case Failure(err) ⇒ errorExit(err.getMessage)
  }
}

object Args {
  type Matcher[+Arg] = PartialFunction[Seq[String], (Args[Arg], Seq[String])]
  def valuedArgument(opt: String): Args[String] = new Args[String] { self ⇒
    val matcher: Matcher[String] = {
      case opt +: value +: tail ⇒ (new Args[String] {
        val matcher = self.matcher
        val get: Try[String] = Success(opt)
      }, tail)
    }
    def get: Try[String] = {
      Failure(new NoSuchElementException("no value applied"))
    }
  }

  case class UserArg(
    user: Option[String] = None,
    pass: Option[String] = None) extends Args[User] {
    val matcher: Matcher[User] = {
      case "--pass" +: pass +: tail ⇒ (copy(pass = Some(pass)), tail)
      case "--user" +: user +: tail ⇒ (copy(user = Some(user)), tail)
    }

    def get: Try[User] = (user, pass) match {
      case (Some(u), Some(p)) ⇒ Success(User(u, p))
      case _ ⇒ Failure(
        new Throwable("username and password for 3w3n.com are required"))
    }
  }

  case class ConfigArg(configPath: String = cwd + "/db.json")
    extends Args[DBConfig] {
    val matcher: Matcher[DBConfig] = {
      case "--config" +: config +: tail ⇒ (copy(configPath = config), tail)
    }

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
    extends Args[IndexedSeq[DateTime]] {

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
    val matcher: Matcher[IndexedSeq[DateTime]] = {
      case "--from" +: f +: tail ⇒ (copy(from = Some(f)), tail)
      case "--to" +: t +: tail ⇒ (copy(to = Some(t)), tail)
      case date +: tail if !date.startsWith("--") ⇒
        (copy(dates = dates :+ date), tail)
    }

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
}

