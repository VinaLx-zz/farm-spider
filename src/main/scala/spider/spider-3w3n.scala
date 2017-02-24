package spider

import Spider._
import java.util.Calendar
import java.util.Calendar.{ YEAR, MONTH, DAY_OF_MONTH }
import java.net.HttpCookie
import spider.util.MD5Hash
import scalaj.http._
import org.json4s._
import org.json4s.native.JsonMethods
import org.json4s.JsonDSL._

object URLs {
  val BASE = "http://www.3w3n.com/"

  val INDEX = "index/goIndex"
  val LOGIN = "loginUser"
  val PRICE_INDEX = "user/price4Day/goIndex"
  val SHOW_PRODUCT_LIST = "user/price4Day/showPriceListPage"
  val SHOW_STAT = "showPriceCount"
  val SHOW_TYPE_LIST = "getProductList"
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
    hash: Option[String] = None,
    cookies: Seq[HttpCookie] = Nil,
    categoryIds: IndexedSeq[Int] = IndexedSeq.empty[Int],
    typeIds: IndexedSeq[(Int, String)] = IndexedSeq.empty[(Int, String)])

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
    for {
      _ ← login(username, password)
      _ ← initHashAndCategoryIds
      state ← getState[State3w3n]
      _ ← initTypeIdsFrom(state.categoryIds)
    } yield ()
  }

  private def login(
    username: String,
    password: String): Spider3w3n[HttpResponse[String]] = {
    Spider.post[State3w3n](URLs.BASE + URLs.LOGIN)(
      Seq("userId" -> username, "password" -> password))
      .flatMap(resp ⇒ Spider { _ ⇒ (resp, State3w3n(cookies = resp.cookies)) })
  }

  private def initHashAndCategoryIds: Spider3w3n[Unit] = {
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

  private def priceIndexPage: Spider3w3n[String] = {
    getState[State3w3n] flatMap (s ⇒ get[State3w3n](
      URLs.BASE + URLs.PRICE_INDEX)(cookies = s.cookies).map(_.body))
  }

  private def parseTypeListJson(j: String): IndexedSeq[(Int, String)] = {
    (for {
      JObject(fields) ← JsonMethods.parse(j)
      JField("id", JInt(id)) ← fields
      JField("type", JString(name)) ← fields
    } yield (id, name))
      .map { case (bigint, name) ⇒ (bigint.intValue, name) }
      .toIndexedSeq
  }

  def productTypeIds(parentId: Int): Spider3w3n[IndexedSeq[(Int, String)]] = {
    Spider.get[State3w3n](
      URLs.BASE + URLs.SHOW_TYPE_LIST)(
        params = List("pId" -> parentId.toString))
      .map(resp ⇒ parseTypeListJson(resp.body))
  }

  private def getCategoryIds(page: String): IndexedSeq[Int] = {
    val extractorRegex = """getProductListByPid\((\d+),""".r
    (extractorRegex findAllMatchIn page map (_.group(1).toInt)).toIndexedSeq
  }

  def initTypeIdsFrom(
    categoryIds: IndexedSeq[Int]): Spider3w3n[Unit] = {
    val spiderSeq = categoryIds.map(productTypeIds(_))
    for {
      seqseq ← sequence(spiderSeq)
      state ← getState[State3w3n]
      _ ← setState(state.copy(typeIds = seqseq.flatten))
    } yield ()
  }

  /**
   * should return Spider3w3n[IndexedSeq[FarmRecord]]
   */
  def getProductOfType(typeId: Int) = {
    for {
      state <- getState[State3w3n]

    }
  }

  def getOne(
    pageNo: Int, typeId: Int, date: Calendar = Calendar.getInstance) = {
    Spider[State3w3n, Option[ShowListParam]] { s ⇒
      s match {
        case State3w3n(None, _, _, _) ⇒ (None, s)
        case State3w3n(Some(hash), cookies, _, _) ⇒
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