package chapter5

import scala.annotation.tailrec

trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  // ex 5.1
  def toList: List[A] = this match {
    case Empty => List.empty[A]
    case Cons(h, t) => h() :: t().toList
  }

  // ex5.2
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => Stream.cons(h(), t().take(n - 1))
    case _ => Stream.empty[A]
  }


  // ex5.2
  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  // ex5.3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Stream.cons(h(), t().takeWhile(p))
    case _ => Stream.empty[A]
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists2(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  // ex5.4
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  // ex5.5 自信なし。全要素調べてるけどいいのかこれ。
  def takeWhileByFoldRight(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((a, b) => if (p(a)) Stream.cons(a, b) else b)

  // ex5.6 bガン無視でいけると思うんだけど 非正格じゃないってこと？
  def headOptionByFoldRight: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  // ex5.7
  def map[B](f: (A => B)): Stream[B] =
    foldRight(Stream.empty[B])((a, b) => Stream.cons(f(a), b))

  def filter(f: (A => Boolean)): Stream[A] =
    foldRight(Stream.empty[A])((a, b) => if (f(a)) Stream.cons(a, b) else b)

  def append[X >: A](x: Stream[X]): Stream[X] =
    foldRight(x)((a, b) => Stream.cons(a, b))


  // foldRight を二回やってるので効率が残念なことになってそう。でもappend使う以外思いつかない
  def flatMap[B](f: (A => Stream[B])): Stream[B] =
    foldRight(Stream.empty[B])(f(_).append(_))

}

case object Empty extends Stream[Nothing]

// case class は名前渡しができないらしい。なんでだろう
// 名前渡しは private[this] val x: => X と 同じで、case class だとコンパニオンオブジェクトを生成するから x にアクセスできなくなっちゃうのでダメってことかしら / functional programming - Scala case class prohibits call-by-name parameters? - Stack Overflow stackoverflow.com/questions/2674…
// Show this thread
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head: A = hd
    lazy val tail: Stream[A] = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

}

