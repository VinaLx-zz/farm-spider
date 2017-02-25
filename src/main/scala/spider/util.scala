package spider.util

import java.security.MessageDigest
import java.text.SimpleDateFormat
import com.github.nscala_time.time.Imports._
import scala.math.ceil

object MD5Hash {
  def apply(s: String): String = {
    val md5 = MessageDigest.getInstance("md5")
    md5.digest(s.getBytes).map(b ⇒ "%02X".format(b)).mkString
  }
}

object DateTimeUtil {
  def fromFormatString(date: String, format: String): DateTime = {
    DateTimeFormat.forPattern(format).parseDateTime(date)
  }
  def toFormatString(date: DateTime, format: String = "yyyy-MM-dd"): String = {
    DateTimeFormat.forPattern(format).print(date)
  }
  def trivialInterval: Interval = {
    val date = DateTime.now
    date to date
  }
}