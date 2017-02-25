package spider

import java.net.HttpCookie
import scalaj.http._
import scala.io.Source

/**
 * state monad
 */
trait Spider[S, +A] {

  import Spider._

  def run(s: S): (A, S) = Spider.run(this, s)

  def flatMap[B](f: A ⇒ Spider[S, B]): Spider[S, B] = FlatMap(this, f)

  def map[B](f: A ⇒ B): Spider[S, B] = {
    flatMap(a ⇒ unit(f(a)))
  }

  def map2[B, C](sb: ⇒ Spider[S, B])(f: (A, B) ⇒ C): Spider[S, C] = {
    flatMap(a ⇒ sb map (b ⇒ f(a, b)))
  }
}

case class Value[S, A](thunk: () ⇒ A) extends Spider[S, A]
case class Transition[S, A](f: S ⇒ (A, S)) extends Spider[S, A]
case class FlatMap[S, A, B](
  spider: Spider[S, A], f: A ⇒ Spider[S, B]) extends Spider[S, B]

object Spider {

  def apply[S, A](a: ⇒ A): Spider[S, A] = unit(a)
  def apply[S, A](f: S ⇒ (A, S)): Spider[S, A] = Transition(f)

  // trampoline to avoid stack overflow
  @annotation.tailrec
  final def run[S, A](spider: Spider[S, A], s: S): (A, S) = spider match {
    case Value(thunk) ⇒ (thunk(), s)
    case Transition(f) ⇒ f(s)
    case FlatMap(spider2, f) ⇒ spider2 match {
      case Value(thunk) ⇒ run(f(thunk()), s)
      case Transition(f2) ⇒
        val (a, s1) = f2(s)
        run(f(a), s1)
      case FlatMap(spider3, f2) ⇒
        run(spider3 flatMap (v ⇒ f2(v) flatMap f), s)
    }
  }
  def unit[S, A](a: ⇒ A): Spider[S, A] = Value(() ⇒ a)

  def getState[S]: Spider[S, S] = Spider[S, S]((s: S) ⇒ (s, s))
  def setState[S](s: S) = Spider[S, Unit]((_: S) ⇒ ((), s))

  def changeState[S](f: S ⇒ S): Spider[S, Unit] = {
    for {
      state ← getState[S]
      _ ← setState(f(state))
    } yield ()
  }

  def sequence[S, A](as: IndexedSeq[Spider[S, A]]): Spider[S, IndexedSeq[A]] = {
    as.foldLeft(unit[S, IndexedSeq[A]](IndexedSeq.empty)) { (acc, sa) ⇒
      acc.map2(sa)(_ :+ _)
    }
  }

  def sequence[S, A](as: List[Spider[S, A]]): Spider[S, List[A]] = {
    as.foldRight(unit[S, List[A]](Nil)) { (sa, acc) ⇒
      sa.map2(acc)(_ :: _)
    }
  }

  def sequence[S, A](as: Stream[Spider[S, A]]): Spider[S, Stream[A]] = {
    as.foldRight(unit[S, Stream[A]](Stream.empty[A])) { (sa, acc) ⇒
      sa.map2(acc)(_ #:: _)
    }
  }

  def post[S](url: String)(
    form: Seq[(String, String)] = Nil,
    headers: Seq[(String, String)] = Nil) = unit[S, HttpResponse[String]] {
    Http(url).postForm(form).headers(headers).asString
  }

  def get[S](url: String)(
    params: Seq[(String, String)] = Nil,
    headers: Seq[(String, String)] = Nil,
    cookies: Seq[HttpCookie] = Nil) = unit[S, HttpResponse[String]] {
    Http(url).params(params).headers(headers).cookies(cookies).asString
  }

}