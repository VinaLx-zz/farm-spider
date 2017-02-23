package spider

import java.net.HttpCookie
import scalaj.http._
import scala.io.Source

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

  def changeState(f: S ⇒ S): Spider[S, A] = Spider[S, A] { s ⇒
    val (a, s1) = run(s)
    (a, f(s1))
  }

}

object Spider {
  def unit[S, A](a: ⇒ A): Spider[S, A] = Spider[S, A] { s ⇒ (a, s) }

  def post[S](url: String)(
    form: Seq[(String, String)] = Nil,
    headers: Seq[(String, String)] = Nil) = unit[S, HttpResponse[String]] {
    Http(url).postForm(form).headers(headers).asString
  }

  def get[S](url: String)(
    params: Seq[(String, String)] = Nil,
    headers: Seq[(String, String)] = Nil,
    cookies: Seq[HttpCookie] = Nil) = unit[S, HttpResponse[String]] {
    println("in get")
    params foreach println
    Http(url).params(params).headers(headers).cookies(cookies).asString
  }

}