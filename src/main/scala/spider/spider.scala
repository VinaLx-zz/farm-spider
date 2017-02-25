package spider

import java.net.HttpCookie
import scalaj.http._
import scala.io.Source

/**
 * state monad
 */
case class Spider[S, +A](run: S ⇒ (A, S)) {

  import Spider._

  def apply(s: S): (A, S) = run(s)

  def flatMap[B](f: A ⇒ Spider[S, B]): Spider[S, B] = Spider[S, B] { s ⇒
    val (a, s1) = run(s)
    f(a).run(s1)
  }

  def map[B](f: A ⇒ B): Spider[S, B] = {
    flatMap(a ⇒ unit(f(a)))
  }

  def map2[B, C](sb: Spider[S, B])(f: (A, B) ⇒ C): Spider[S, C] = {
    flatMap(a ⇒ sb map (b ⇒ f(a, b)))
  }

  def changeState(f: S ⇒ S): Spider[S, A] = Spider[S, A] { s ⇒
    val (a, s1) = run(s)
    (a, f(s1))
  }
}

object Spider {
  def unit[S, A](a: ⇒ A): Spider[S, A] = Spider[S, A](s ⇒ (a, s))

  def getState[S]: Spider[S, S] = Spider[S, S](s ⇒ (s, s))
  def setState[S](s: S) = Spider[S, Unit](_ ⇒ ((), s))

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