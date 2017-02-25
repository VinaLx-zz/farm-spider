package spider
package spider3w3n

import Spider._
import spider.util.{ MD5Hash, StringToCalendar, CalendarToString }

import scala.math.{ ceil, floor }
import java.util.Calendar
import java.util.Calendar.{ YEAR, MONTH, DAY_OF_MONTH }
// http support
import java.net.HttpCookie
import scalaj.http.HttpResponse
// json parsing
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods
// html parsing
import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._

import Combinators._
import TextProcessing._

object URLs {
  val BASE = "http://www.3w3n.com/"

  val INDEX = "index/goIndex"
  val LOGIN = "loginUser"
  val PRICE_INDEX = "user/price4Day/goIndex"
  val PRODUCT_TABLE = "user/price4Day/showPriceListPage"
  val SHOW_STAT = "showPriceCount"
  val SHOW_TYPE_LIST = "getProductList"
}

case class ProductTableParam(
  pageNo: Int, typeId: Int, date: Calendar = Calendar.getInstance) {
  def toSequence: Seq[(String, String)] = {
    val dateStr = "%d-%02d-%02d".format(
      date.get(YEAR), date.get(MONTH) + 1, date.get(DAY_OF_MONTH))
    Seq(
      "pageNo" -> pageNo.toString,
      "typeId" -> typeId.toString,
      "date" -> dateStr)
  }
}

case class WorkerTag(index: Int, total: Int) {
  def toIndexRange(jobs: Int): (Int, Int) = {
    val chunk = jobs.toDouble / total
    // avoid round off error
    if (index == 1) (0, floor(chunk).toInt)
    else if (index == total) (floor(jobs - chunk).toInt, jobs)
    else (floor((index - 1) * chunk).toInt, floor(index * chunk).toInt)
  }
}

case class User(username: String, password: String)

case class State3w3n(
  hash: Option[String] = None,
  cookies: Seq[HttpCookie] = Nil,
  categoryIds: IndexedSeq[Int] = IndexedSeq.empty[Int],
  typeIds: IndexedSeq[(Int, String)] = IndexedSeq.empty[(Int, String)],
  tag: WorkerTag = WorkerTag(index = 1, total = 1),
  user: Option[User] = None)

case class ProductTableRecord(
  name: String,
  price: Double,
  market: String,
  infoSource: String,
  date: Calendar) {
  override def toString: String = {
    s"$name - $price - $market - $infoSource - ${CalendarToString(date)}"
  }
}

object Combinators {
  type Spider3w3n[+A] = Spider[State3w3n, A]

  def startOne(
    username: String, password: String, tag: WorkerTag): Spider3w3n[Unit] = {
    for {
      _ ← allocateOne(tag)
      _ ← init(username, password)
      state ← getState[State3w3n]
      (start, end) = state.tag.toIndexRange(state.typeIds.size)
      _ ← getProductsAndSink(state.typeIds.view.slice(start, end))
    } yield ()
  }

  def allocateOne(tag: WorkerTag = WorkerTag(1, 1)): Spider3w3n[Unit] = {
    setState(State3w3n(tag = tag))
  }

  def init(username: String, password: String): Spider3w3n[Unit] = {
    for {
      _ ← login(username, password)
      _ ← setUser(username, password)
      _ ← initHashAndCategoryIds
      state ← getState[State3w3n]
      (start, end) = state.tag.toIndexRange(state.categoryIds.size)
      _ ← initTypeIdsFrom(state.categoryIds.view.slice(start, end))
    } yield ()
  }

  def relogin: Spider3w3n[Unit] = {
    for {
      state ← getState[State3w3n]
      Some(User(username, password)) = state.user
      _ ← login(username, password)
      page ← priceIndexPage
      _ ← setHash(page)
    } yield ()
  }

  private def login(username: String, password: String): Spider3w3n[Unit] = {
    for {
      state ← getState[State3w3n]
      resp ← Spider.post[State3w3n](URLs.BASE + URLs.LOGIN)(
        Seq("userId" -> username, "password" -> password))
      _ ← setState(state.copy(cookies = resp.cookies))
    } yield ()
  }

  private def setUser(username: String, password: String): Spider3w3n[Unit] = {
    changeState[State3w3n](_.copy(user = Some(User(username, password))))
  }

  private def initHashAndCategoryIds: Spider3w3n[Unit] = {
    for {
      page ← priceIndexPage
      _ ← setHash(page)
      _ ← setCategoryIds(page)
    } yield ()
  }

  private def priceIndexPage: Spider3w3n[String] = {
    for {
      state ← getState[State3w3n]
      resp ← get[State3w3n](
        URLs.BASE + URLs.PRICE_INDEX)(cookies = state.cookies)
    } yield resp.body
  }

  private def setHash(page: String): Spider3w3n[Unit] = {
    changeState[State3w3n](_.copy(hash = extractHash(page)))
  }

  private def setCategoryIds(page: String): Spider3w3n[Unit] = {
    changeState[State3w3n](_.copy(categoryIds = extractCategoryIds(page)))
  }

  def productTypeIds(parentId: Int): Spider3w3n[IndexedSeq[(Int, String)]] = {
    Spider.get[State3w3n](
      URLs.BASE + URLs.SHOW_TYPE_LIST)(
        params = List("pId" -> parentId.toString))
      .map(resp ⇒ parseTypeListJson(resp.body))
  }

  def initTypeIdsFrom(categoryIds: Seq[Int]): Spider3w3n[Unit] = {
    val spiderSeq = categoryIds.view.map(productTypeIds(_)).toIndexedSeq
    for {
      seqseq ← sequence(spiderSeq)
      _ ← changeState[State3w3n](_.copy(typeIds = seqseq.flatten))
    } yield ()
  }

  /**
   * @return Spider3w3n[Unit]
   */
  def getProductsAndSink(productIds: Seq[(Int, String)]) = {
    val spiderSeq = productIds.view.map { (t: (Int, String)) ⇒
      getProductOfType(t._1).map(records ⇒ (t._2, records))
    }.map(_ flatMap (record ⇒ sink(record))).toIndexedSeq
    sequence(spiderSeq).map(_ ⇒ ())
  }

  def sink(record: (String, IndexedSeq[ProductTableRecord])): Spider3w3n[Unit] = {
    def print(record: (String, IndexedSeq[ProductTableRecord])): Unit = {
      println(record._1)
      record._2 foreach println
    }
    unit(print(record))
  }

  /**
   * @return Spider3w3n[IndexedSeq[FarmRecord]]
   */
  def getProductOfType(typeId: Int, date: Calendar = Calendar.getInstance) = {
    def go(
      acc: Spider3w3n[IndexedSeq[ProductTableRecord]],
      pageNo: Int,
      date: Calendar): Spider3w3n[IndexedSeq[ProductTableRecord]] = {
      val params = ProductTableParam(
        pageNo = pageNo, typeId = typeId, date = date)
      val thisPage = productList(params)
      acc flatMap (before ⇒ thisPage flatMap { current ⇒
        if (current.isEmpty) unit(before)
        else go(unit(before ++ current), pageNo + 1, date)
      })
    }
    go(unit(IndexedSeq.empty), 1, date)
  }

  private def productList(
    params: ProductTableParam): Spider3w3n[IndexedSeq[ProductTableRecord]] = {
    def handleResponse(
      resp: HttpResponse[String]): Spider3w3n[IndexedSeq[ProductTableRecord]] = {
      if (resp.isSuccess) unit(parseProductTable(resp.body))
      // if login status expired, relogin and retry
      else if (resp.isRedirect) relogin flatMap (_ ⇒ productList(params))
      else {
        // shouldn't go here in normal case
        assert(false)
        (sys.exit(1): Spider3w3n[IndexedSeq[ProductTableRecord]])
      }
    }
    for {
      state ← getState[State3w3n]
      resp ← get(URLs.BASE + URLs.PRODUCT_TABLE)(
        cookies = state.cookies,
        params = params.toSequence :+ ("r" -> state.hash.get))
      res ← handleResponse(resp)
    } yield res
  }

}

object TextProcessing {
  private[spider3w3n] def extractHash(page: String): Option[String] = {
    val extractorRegex = """enc\('(\w+?)'\)""".r
    extractorRegex findFirstMatchIn page map (_.group(1)) map (MD5Hash(_))
  }

  private[spider3w3n] def extractCategoryIds(page: String): IndexedSeq[Int] = {
    val extractorRegex = """getProductListByPid\((\d+),""".r
    (extractorRegex findAllMatchIn page map (_.group(1).toInt)).toIndexedSeq
  }

  private[spider3w3n] def parseTypeListJson(
    j: String): IndexedSeq[(Int, String)] = {
    (for {
      JObject(fields) ← JsonMethods.parse(j)
      JField("id", JInt(id)) ← fields
      JField("type", JString(name)) ← fields
    } yield (id, name))
      .map { case (bigint, name) ⇒ (bigint.intValue, name) }
      .toIndexedSeq
  }

  private[spider3w3n] def parseProductTable(
    page: String): IndexedSeq[ProductTableRecord] = {
    val table = JsoupBrowser().parseString(page) >> elementList("tr")
    if (table.isEmpty) return IndexedSeq.empty
    val recordList =
      table.tail map (_ >> texts("td")) map {
        (fields: Iterable[String]) ⇒
          val cols = fields.toIndexedSeq
          val price = cols(1).trim.split("""[^\d\.]""", 2)(0).toDouble
          val date = StringToCalendar("yyyy-MM-dd")(cols(4))
          ProductTableRecord(
            name = cols(0),
            price = price,
            market = cols(2),
            infoSource = cols(3),
            date = date)
      }
    recordList.toIndexedSeq
  }
}

