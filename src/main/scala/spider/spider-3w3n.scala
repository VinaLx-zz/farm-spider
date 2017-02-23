package spider

import Spider._
import java.util.Calendar
import java.util.Calendar.{ YEAR, MONTH, DAY_OF_MONTH }
import java.net.HttpCookie
import spider.util._
import scalaj.http._

object URLs {
  val BASE = "http://www.3w3n.com/"

  val INDEX = "index/goIndex"
  val LOGIN = "loginUser"
  val PRICE_INDEX = "user/price4Day/goIndex"
  val SHOW_PRODUCT_LIST = "user/price4Day/showPriceListPage"
  val SHOW_STAT = "showPriceCount"
  val SHOW_PRODUCT_ID = "getProductList"
}

object Spider3w3n {

  case class ShowListParam(
    pageNo: Int, typeId: Int, date: Calendar, hash: String) {
    def toSequence: Seq[(String, String)] = {
      val dateStr = "%d-%02d-%02d".format(
        date.get(YEAR), date.get(MONTH) + 1, date.get(DAY_OF_MONTH))
      Seq("pageNo" -> pageNo.toString, "typeId" -> typeId.toString,
        "date" -> dateStr, "r" -> hash)
    }
  }

  case class State3w3n(
    hash: Option[String] = None, cookies: Seq[HttpCookie] = Nil,
    categoryIds: IndexedSeq[Int] = IndexedSeq[Int]())

  type Spider3w3n[+A] = Spider[State3w3n, A]

  implicit class Spider3w3nOps[+A](spider: Spider3w3n[A]) {
    def refresh(username: String, password: String): Spider3w3n[A] = {
      spider.flatMap(a ⇒ init(username, password).map(_ ⇒ a))
    }
  }

  /**
   * initializes the state of the spider
   */
  def init(username: String, password: String): Spider3w3n[Unit] = {
    login(username, password).flatMap(_ ⇒ priceIndexHash)
  }

  private def login(
    username: String,
    password: String): Spider3w3n[HttpResponse[String]] = {
    Spider.post[State3w3n](URLs.BASE + URLs.LOGIN)(
      Seq("userId" -> username, "password" -> password))
      .flatMap(resp ⇒ Spider { _ ⇒ (resp, State3w3n(cookies = resp.cookies)) })
  }

  private def priceIndexHash: Spider3w3n[Unit] = {
    priceIndexPage flatMap { page ⇒
      Spider[State3w3n, Unit] { s ⇒
        val newState = s.copy(hash = getHashFromPage(page))
        if (s.categoryIds.isEmpty)
          ((), newState.copy(categoryIds = getCategoryIds(page)))
        else ((), newState)
      }
    }
  }

  private def getHashFromPage(page: String): Option[String] = {
    val extractorRegex = """enc\('(\w+?)'\)""".r
    extractorRegex findFirstMatchIn page map (_.group(1)) map (MD5Hash(_))
  }

  private def priceIndexPage: Spider3w3n[String] = Spider { s ⇒
    Spider.get[State3w3n](
      URLs.BASE + URLs.PRICE_INDEX)(cookies = s.cookies).map(_.body).run(s)
  }

  private def getCategoryIds(page: String): IndexedSeq[Int] = {
    val extractorRegex = """getProductListByPid\((\d+),""".r
    (extractorRegex findAllMatchIn page map (_.group(1).toInt)).toIndexedSeq
  }

  def getOne(
    pageNo: Int, typeId: Int, date: Calendar = Calendar.getInstance) = {
    Spider[State3w3n, Option[ShowListParam]] { s ⇒
      s match {
        case State3w3n(None, _, _) ⇒ (None, s)
        case State3w3n(Some(hash), cookies, _) ⇒
          (Some(ShowListParam(pageNo, typeId, date, hash)), s)
      }
    }.flatMap(po ⇒ productList(po))
  }

  def productList(params: Option[ShowListParam]): Spider3w3n[Option[String]] =
    params match {
      case None ⇒ Spider.unit(None)
      case Some(p) ⇒ Spider { s ⇒
        Spider.get[State3w3n](URLs.BASE + URLs.SHOW_PRODUCT_LIST)(
          params = p.toSequence, cookies = s.cookies)
          .map(resp ⇒ Some(resp.body)).run(s)
      }
    }
}