package spider
package spider3w3n

import java.io._

import spider.util.DateTimeUtil.{
  splitInterval,
  trivialInterval,
  toFormatString
}

import scala.concurrent.{ Future, ExecutionContext, Await }
import scala.concurrent.duration.Duration.Inf
import scala.util.{ Try, Success, Failure }

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
}

object Runner {
  def initSpiders(
    user: User,
    period: Interval = trivialInterval,
    sink: Sinker = simpleSink,
    slices: Int = 4): List[Spider3w3n[Unit]] = {
    val datesList = splitInterval(1.day)(period, slices)
    val streams = for {
      i ← 0 until datesList.size
    } yield new FileOutputStream(s"$i.txt")
    datesList zip streams map {
      case (dates, s) ⇒
        getSpider(user, dates.toIndexedSeq, toOutputStream(s) _)
    }
    // datesList map (dates ⇒ getSpider(user, dates.toIndexedSeq, sink))
  }

  def runSpiderAsync(s: Spider3w3n[Unit])(
    implicit ec: ExecutionContext): Future[Unit] = Future {
    s run State3w3n()
  }

  def runAll(l: List[Spider3w3n[Unit]])(implicit ec: ExecutionContext): Unit = {
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

  def parseArgs(args: Array[String]): (User, Interval, Int) = {
    if (args.size < 2) {
      println("usage: run <username> <password> [period]")
      sys.exit(1)
    }
    val user = User(args(0), args(1))
    val currentTime = DateTime.now
    val day = if (args.size == 3) args(2).toInt - 1 else 0
    val interval = (currentTime - day.day) to currentTime
    (user, interval, 4)
  }

  def go(args: Array[String]): Unit = {
    import scala.concurrent.ExecutionContext.Implicits._
    val (user, interval, worker) = parseArgs(args)
    val spiders = initSpiders(user, interval, toOutputStream(System.out))
    runAll(spiders)
  }

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis
    go(args)
    val end = System.currentTimeMillis
    println(
      s"spend ${(end - start).toDouble / 1000} seconds")
  }
}