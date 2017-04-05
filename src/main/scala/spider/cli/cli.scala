package spider.cli

import command._
import spider.Util.errorExit

import com.typesafe.scalalogging.Logger

object CLI {

  lazy val logger = Logger("spider.cli")

  def go(args: Array[String]): Unit = args.toSeq match {
    case e if args.isEmpty ⇒ Help(args)
    case "scrape" +: tail ⇒ Scrape(tail)
    case "wait" +: tail ⇒ Wait(tail)
    case "help" +: tail ⇒ Help(tail)
    case "remove" +: tail ⇒ Remove(tail)
    case "getcat" +: tail ⇒ GetCategory(tail)
    case other +: tail ⇒ errorExit(s"unknown command $other")
  }
}