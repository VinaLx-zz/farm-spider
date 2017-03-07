package spider
package spider3w3n

import java.io._

import spider.Util.splitSeq
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
    slices: Int = 10): List[Spider3w3n[Unit]] = {
    val datesList = splitSeq(period, slices)
    datesList map (dates ⇒ getSpider(user, dates.toIndexedSeq, sink))
  }

  def runSpiderAsync(s: Spider3w3n[Unit])(
    implicit ec: ExecutionContext): Future[Unit] = Future {
    s run State3w3n()
  }

  def runSpiders(l: List[Spider3w3n[Unit]]): Unit = {
    implicit val ec = ExecutionContext.fromExecutorService(
      new ForkJoinPool(l.size))
    val fs = l map (s ⇒ runSpiderAsync(s))
    for {
      f ← fs
    } f onComplete {
      case Success(_) ⇒ ()
      case Failure(e) ⇒ e.printStackTrace
    }
    for {
      f ← fs
    } Await.result(f, Inf)
  }
}