package spider

import java.security.MessageDigest
import java.text.SimpleDateFormat
import com.github.nscala_time.time.Imports._
import scala.math.ceil
import scala.util.{ Try, Success, Failure }
import org.joda.time.ReadablePeriod
import java.nio.file.Paths

object Util {
  def md5Hash(s: String): String = {
    val md5 = MessageDigest.getInstance("md5")
    md5.digest(s.getBytes).map(b ⇒ "%02X".format(b)).mkString
  }

  def splitSeq[A](as: Seq[A], n: Int): List[Seq[A]] = {
    (as grouped ceil(as.size.toDouble / n).toInt).toList
  }

  def tryOption[A](a: ⇒ A): Option[A] = Try(a) match {
    case Success(s) ⇒ Some(a)
    case Failure(_) ⇒ None
  }

  def cwd: String = Paths.get(".").toAbsolutePath.toString

  object DateTimeUtil {
    def fromFormatString(date: String, format: String): DateTime = {
      DateTimeFormat.forPattern(format).parseDateTime(date)
    }
    def toFormatString(
      date: DateTime, format: String = "yyyy-MM-dd"): String = {
      DateTimeFormat.forPattern(format).print(date)
    }
    def trivialInterval: Interval = {
      val date = DateTime.now
      date to date
    }
  }
}