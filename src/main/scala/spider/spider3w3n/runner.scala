package spider
package spider3w3n

import java.io._

import spider.Util.splitSeq
import spider.Util.DateTimeUtil.{
  trivialInterval,
  toFormatString
}
import spider.database._
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
        val f = db.run(DBIO.sequence(
          records map { record ⇒ FarmTable.insert(name, record) }))
        Await.result(f, Inf)
      case Failure(e) ⇒ ()
    }
  }
}

object Runner {
  def initSpiders(
    user: User,
    period: IndexedSeq[DateTime] = IndexedSeq(DateTime.now),
    sink: Sinker = simpleSink,
    slices: Int = 10): List[Spider3w3n[Unit]] = {
    val datesList = splitSeq(period, slices)
    // val streams = for {
    //   i ← 0 until datesList.size
    // } yield new FileOutputStream(s"$i.txt")
    // datesList zip streams map {
    //   case (dates, s) ⇒
    //     getSpider(user, dates.toIndexedSeq, toOutputStream(s) _)
    // }
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
    val streams = for {
      i ← 0 until l.size
    } yield new FileOutputStream(s"res$i.txt")
    for {
      (f, stream) ← fs zip streams
    } {
      f onComplete {
        case Success(u) ⇒ stream.write("success\n".getBytes)
        case Failure(e) ⇒
          e.printStackTrace(new PrintStream(stream))
      }
    }
    for {
      f ← fs
    } Await.result(f, Inf)
  }

  // def parseArgs(args: Array[String]): (User, Interval, Int) = {
  //   if (args.size < 2) {
  //     println("usage: run <username> <password> [period]")
  //     sys.exit(1)
  //   }
  //   val user = User(args(0), args(1))
  //   val currentTime = DateTime.now
  //   val day = if (args.size == 3) args(2).toInt - 1 else 0
  //   val interval = (currentTime - day.day) to currentTime
  //   (user, interval, 4)
  // }

  // def go(args: Array[String]): Unit = {
  //   import scala.concurrent.ExecutionContext.Implicits._
  //   val (user, interval, worker) = parseArgs(args)
  //   val db = FarmDB.connect("root")
  //   Await.result(
  //     db.run(FarmDB.createProductTable) recover { case e ⇒ () }, Inf)
  //   val spiders = initSpiders(user, interval, writeToDB(db))
  //   runConcurrently(spiders)
  // }

  // def main(args: Array[String]): Unit = {
  //   val start = System.currentTimeMillis
  //   go(args)
  //   val end = System.currentTimeMillis
  //   println(
  //     s"spend ${(end - start).toDouble / 1000} seconds")
  // }
}