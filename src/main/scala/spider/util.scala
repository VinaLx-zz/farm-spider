package spider.util

import java.security.MessageDigest
import java.text.SimpleDateFormat
import java.util.Calendar
import scala.math.ceil

object MD5Hash {
  def apply(s: String): String = {
    val md5 = MessageDigest.getInstance("md5")
    md5.digest(s.getBytes).map(b ⇒ "%02X".format(b)).mkString
  }
}

object StringToCalendar {
  def apply(p: String)(s: String): Calendar = {
    val cal = Calendar.getInstance
    cal.setTime(new SimpleDateFormat(p).parse(s))
    cal
  }
}

object CalendarToString {
  def apply(cal: Calendar, p: String = "yyyy-MM-dd"): String = {
    new SimpleDateFormat(p).format(cal.getTime)
  }
}