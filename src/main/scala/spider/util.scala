package spider.util

import java.security.MessageDigest

object MD5Hash {
  def apply(s: String): String = {
    val md5 = MessageDigest.getInstance("md5")
    md5.digest(s.getBytes).map(b ⇒ "%02X".format(b)).mkString
  }
}