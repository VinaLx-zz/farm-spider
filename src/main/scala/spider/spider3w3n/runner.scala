package spider
package spider3w3n

import java.io._

import spider.Util.{ splitSeq, getStackTraceString }
import spider.Util.DateTimeUtil.{
  trivialInterval,
  toFormatString
}
import spider.database._
import spider.database.FarmDB._

import slick.jdbc.JdbcBackend._
import slick.dbio.DBIO

import scala.concurrent.{ Future, ExecutionContext, Await }
import scala.concurrent.duration.Duration.Inf
import scala.util.{ Try, Success, Failure }
import java.util.concurrent.ForkJoinPool

import com.github.nscala_time.time.Imports._
import com.typesafe.scalalogging.Logger

import Combinators._
import TextProcessing._
import Sinker._

object Sinker {
  type Sinker = (String, Try[IndexedSeq[Record3w3n]]) ⇒ Unit

  def simpleSink(name: String, records: Try[IndexedSeq[Record3w3n]]): Unit = {
    def print(record: (String, Try[IndexedSeq[Record3w3n]])): Unit = {
      println(record._1)
      record._2 match {
        case Success(s) ⇒ s foreach println
        case Failure(e) ⇒ e.printStackTrace()

      }
    }
    print((name, records))
  }

  def toOutputStream(stream: OutputStream)(
    name: String, t: Try[IndexedSeq[Record3w3n]]): Unit = {
    val content: String = t match {
      case Success(records) ⇒
        if (records.nonEmpty)
          s"${toFormatString(records.head.date)} Get"
        else "None"
      case Failure(e) ⇒ throw e
    }
    val s = s"$name: $content\n"
    stream.write(s.getBytes)
  }

  def writeToDB(db: Database): Sinker = { (name, t) ⇒
    t match {
      case Success(records) ⇒
        import scala.concurrent.ExecutionContext.Implicits.global
        db.runSync(DBIO.sequence(
          records map { record ⇒ FarmTable.insert(name, record) }))
      case Failure(e) ⇒ ()
    }
  }
}

object Runner {

  lazy val logger = Logger("spider.runner")

  def go(
    user: User,
    dates: IndexedSeq[DateTime] = IndexedSeq(DateTime.now),
    sink: Sinker = simpleSink,
    parallelism: Int = 1): Unit = {
    runSpiders(initSpiders(user, dates, sink, parallelism))
  }

  def initSpiders(
    user: User,
    period: IndexedSeq[DateTime] = IndexedSeq(DateTime.now),
    sink: Sinker = simpleSink,
    slices: Int): List[Spider3w3n[Unit]] = {
    val datesList = splitSeq(period, slices)
    logger.info(s"split ${period.size} date(s) into ${datesList.size} slice(s)")
    datesList map (dates ⇒ getSpider(user, dates.toIndexedSeq, sink))
  }

  def getCategories(db: Database): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global
    db.runSync(CategoryTable.clearAll)
    (getProductTypes map { categories ⇒
      for {
        ((catId, catName), productIds) ← categories
        _ ← for {
          (productId, productName) ← productIds
        } yield db.runSync(CategoryTable.insert(catName, productName))
      } yield ()
    }).run(State3w3n())
  }

  private def runSpiderAsync(s: Spider3w3n[Unit])(
    implicit ec: ExecutionContext): Future[Unit] = Future {
    s run State3w3n()
  }

  private def waitAll[U](fs: Seq[Future[U]]): Unit = {
    for {
      f ← fs
    } Await.result(f, Inf)
  }

  private def runSpiders(l: List[Spider3w3n[Unit]]): Unit = {
    implicit val ec = ExecutionContext.fromExecutorService(
      new ForkJoinPool(l.size))
    logger.info(s"starting ${l.size} worker(s)")
    val fs = l map (s ⇒ runSpiderAsync(s))
    for {
      (f, id) ← fs zip (Stream.from(1))
    } f onComplete {
      case Success(_) ⇒
        logger.info(s"worker $id complete")
      case Failure(e) ⇒
        logger.error(getStackTraceString(e))
    }
    waitAll(fs)
  }
}