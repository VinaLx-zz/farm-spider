package spider
package spider3w3n

import Spider._
import spider.util.MD5Hash
import spider.util.DateTimeUtil.{
  trivialInterval,
  fromFormatString,
  toFormatString
}

import scala.math.floor
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
// day-time support
import com.github.nscala_time.time.Imports._

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
  pageNo: Int, typeId: Int, date: DateTime = DateTime.now) {
  def toSequence: Seq[(String, String)] = {
    val dateStr = "%d-%02d-%02d".format(
      date.year.get, date.month.get, date.day.get)
    Seq(
      "pageNo" -> pageNo.toString,
      "typeId" -> typeId.toString,
      "date" -> dateStr)
  }
}

case class User(username: String, password: String)

case class State3w3n(
  hash: Option[String] = None,
  cookies: Seq[HttpCookie] = Nil,
  categoryIds: IndexedSeq[Int] = IndexedSeq.empty[Int],
  typeIds: IndexedSeq[(Int, String)] = IndexedSeq.empty[(Int, String)],
  user: Option[User] = None,
  period: Interval = trivialInterval)

case class ProductTableRecord(
  name: String,
  price: Double,
  market: String,
  // infoSource: String,
  date: DateTime) {
  override def toString: String = {
    s"$name - $price - $market  - ${toFormatString(date)}"
  }
}

object Combinators {
  type Spider3w3n[+A] = Spider[State3w3n, A]

  import Setters._

  def startOne(
    user: User,
    period: Interval = trivialInterval): Spider3w3n[Unit] = {
    for {
      _ ← setPeriod(period)
      _ ← init(user)
      state ← getState[State3w3n]
      _ ← getProductsAndSink(state.typeIds, period)
    } yield ()
  }

  def init(user: User): Spider3w3n[Unit] = {
    for {
      _ ← login(user)
      _ ← setUser(user)
      _ ← setHashAndCategoryIds
      state ← getState[State3w3n]
      // TODO: the work should not base on slice of categoryIds
      _ ← setProductIds(state.categoryIds)
    } yield ()
  }

  def login(user: User): Spider3w3n[Unit] = {
    for {
      state ← getState[State3w3n]
      resp ← Spider.post[State3w3n](URLs.BASE + URLs.LOGIN)(
        Seq("userId" -> user.username, "password" -> user.password))
      _ ← setState(state.copy(cookies = resp.cookies))
    } yield ()
  }

  object Setters {
    def setUser(user: User): Spider3w3n[Unit] = {
      changeState(_.copy(user = Some(user)))
    }

    def setPeriod(period: Interval): Spider3w3n[Unit] = {
      changeState(_.copy(period = period))
    }

    def setHashAndCategoryIds: Spider3w3n[Unit] = {
      for {
        page ← getPriceIndexPage
        _ ← setHashFromPage(page)
        _ ← setCategoryIdsFromPage(page)
      } yield ()
    }

    def setHashFromPage(page: String): Spider3w3n[Unit] = {
      changeState[State3w3n](_.copy(hash = extractHash(page)))
    }

    def setCategoryIdsFromPage(page: String): Spider3w3n[Unit] = {
      changeState[State3w3n](_.copy(categoryIds = extractCategoryIds(page)))
    }

    def setProductIds(categoryIds: Seq[Int]): Spider3w3n[Unit] = {
      val spiderSeq = categoryIds.view.map(
        getProductIdsFromCategoryId(_)).toIndexedSeq
      for {
        seqseq ← sequence(spiderSeq)
        _ ← changeState[State3w3n](_.copy(typeIds = seqseq.flatten))
      } yield ()
    }
  }

  def relogin: Spider3w3n[Unit] = {
    for {
      state ← getState[State3w3n]
      // TODO: Add error handling
      _ ← login(state.user.get)
      page ← getPriceIndexPage
      _ ← setHashFromPage(page)
    } yield ()
  }

  def getPriceIndexPage: Spider3w3n[String] = {
    for {
      state ← getState[State3w3n]
      resp ← get[State3w3n](
        URLs.BASE + URLs.PRICE_INDEX)(cookies = state.cookies)
    } yield resp.body
  }

  def getProductIdsFromCategoryId(
    categoryId: Int): Spider3w3n[IndexedSeq[(Int, String)]] = {
    Spider.get[State3w3n](
      URLs.BASE + URLs.SHOW_TYPE_LIST)(
        params = List("pId" -> categoryId.toString))
      .map(resp ⇒ parseTypeListJson(resp.body))
  }

  /**
   * @return Spider3w3n[Unit]
   */
  def getProductsAndSink(
    productIds: Seq[(Int, String)], period: Interval = trivialInterval) = {
    val spiderStream = for {
      // TODO: compute period lazily without stack overflow
      date <- (period by[IndexedSeq] 1.day)
      (id, t) <- productIds 
    } yield (getProductOfType(id, date) flatMap( records => sink(t, records)))
    sequence(spiderStream).map(_ ⇒ ())
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
  def getProductOfType(typeId: Int, date: DateTime = DateTime.now) = {
    def go(
      acc: Spider3w3n[IndexedSeq[ProductTableRecord]],
      pageNo: Int,
      date: DateTime): Spider3w3n[IndexedSeq[ProductTableRecord]] = {
      val params = ProductTableParam(
        pageNo = pageNo, typeId = typeId, date = date)
      val thisPage = getProductRecords(params)
      acc flatMap { before ⇒ 
        thisPage flatMap { current ⇒
          if (current.isEmpty) unit(before)
          else go(unit(before ++ current), pageNo + 1, date)
        }
      }
    }
    go(unit(IndexedSeq.empty), 1, date)
  }

  def getProductRecords(
    params: ProductTableParam): Spider3w3n[IndexedSeq[ProductTableRecord]] = {
    def handleResponse(
      resp: HttpResponse[String]): Spider3w3n[IndexedSeq[ProductTableRecord]] = {
      if (resp.isSuccess) unit(parseProductTable(resp.body))
      // if login status expired, relogin and retry
      else if (resp.isRedirect) relogin flatMap (_ ⇒ getProductRecords(params))
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
          val date = fromFormatString(cols(3), "yyyy-MM-dd")
          ProductTableRecord(
            name = cols(0),
            price = price,
            market = cols(2),
            // infoSource = cols(3),
            date = date)
      }
    recordList.toIndexedSeq
  }

  def main(args: Array[String]): Unit = {
    if (args.size < 2) {
      println("usage: run <username> <password> [period]")
      return ()
    }
    val user = User(args(0), args(1))
    val currentTime = DateTime.now

    val d = if (args.size == 3) args(2).toInt - 1 else 0
    val interval = (currentTime - d.day) to currentTime

    val start = System.currentTimeMillis
    val s = startOne(user, interval)
    s run State3w3n()
    val end = System.currentTimeMillis
    println(s"spend ${(end - start).toDouble / 1000} seconds for  days records")

  }
}

