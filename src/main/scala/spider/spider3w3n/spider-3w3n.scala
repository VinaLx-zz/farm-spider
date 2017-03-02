package spider
package spider3w3n

import Spider._

import spider.Util.DateTimeUtil.{
  trivialInterval,
  toFormatString
}

import scala.util.{ Try, Success, Failure }

// http support
import java.net.HttpCookie
import scalaj.http.HttpResponse

// day-time support
import com.github.nscala_time.time.Imports._

import Combinators._
import TextProcessing._
import Sinker._

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
  user: Option[User] = None)

case class Record3w3n(
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

  def getSpider(
    user: User,
    days: IndexedSeq[DateTime],
    sink: Sinker): Spider3w3n[Unit] = {
    for {
      _ ← init(user)
      state ← getState[State3w3n]
      _ ← getProductsAndSink(state.typeIds, days, sink)
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
    } yield resp.get.body
  }

  def getProductIdsFromCategoryId(
    categoryId: Int): Spider3w3n[IndexedSeq[(Int, String)]] = {
    Spider.get[State3w3n](
      URLs.BASE + URLs.SHOW_TYPE_LIST)(
        params = List("pId" -> categoryId.toString))
      .map { resp ⇒ parseTypeListJson(resp.get.body) }
  }

  /**
   * @return Spider3w3n[Unit]
   */
  def getProductsAndSink(
    productIds: Seq[(Int, String)],
    dates: IndexedSeq[DateTime],
    sink: Sinker) = {
    val spiderSeq = for {
      date ← dates
      (id, t) ← productIds
    } yield (getProductOfType(id, date) map (records ⇒ sink.tupled(t, records)))
    sequence(spiderSeq).map(_ ⇒ ())
  }

  def getProductOfType(typeId: Int, date: DateTime = DateTime.now) = {
    def go(
      acc: Spider3w3n[Try[IndexedSeq[Record3w3n]]],
      pageNo: Int,
      date: DateTime): Spider3w3n[Try[IndexedSeq[Record3w3n]]] = {
      val params = ProductTableParam(
        pageNo = pageNo, typeId = typeId, date = date)
      val thisPage = getProductRecords(params)
      acc flatMap { before ⇒
        thisPage flatMap {
          case Success(current) ⇒
            if (current.isEmpty) unit(before)
            else go(unit(before map (_ ++ current)), pageNo + 1, date)
          case fail ⇒ unit(fail)
        }
      }
    }
    go(unit(Success(IndexedSeq.empty)), 1, date)
  }

  def getProductRecords(
    params: ProductTableParam): Spider3w3n[Try[IndexedSeq[Record3w3n]]] = {
    def handleResponse(
      r: Try[HttpResponse[String]]): Spider3w3n[Try[IndexedSeq[Record3w3n]]] =
      r match {
        case Success(resp) ⇒
          if (resp.isSuccess) unit(Try(parseProductTable(resp.body)))
          // if login status expired, relogin and retry
          else if (resp.isRedirect)
            relogin flatMap { _ ⇒ getProductRecords(params) }
          else
            // shouldn't go here in normal case
            unit(Try { assert(false, resp); sys.exit(1) })
        case Failure(e) ⇒
          unit(Failure[IndexedSeq[Record3w3n]](
            new Throwable("http error when getting table", e)))
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
