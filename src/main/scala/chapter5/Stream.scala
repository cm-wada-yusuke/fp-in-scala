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
    foldRight(x)(Stream.cons(_, _))


  // foldRight を二回やってるので効率が残念なことになってそう。でもappend使う以外思いつかない
  def flatMap[B](f: (A => Stream[B])): Stream[B] =
    foldRight(Stream.empty[B])(f(_).append(_))

  def find(p: A => Boolean): Option[A] =
    filter(p).headOption

  // ex 5.13
  def unfoldMap[B](f: (A => B)): Stream[B] =
    Stream.unfold(this)((s: Stream[A]) => s.headOption.map(a => (f(a), s.drop(1))))

  // もうちょっとスートにかけそうなんだけど…
  def unfoldTake(n: Int): Stream[A] =
    Stream.unfold((this, n))(s => if (s._2 <= 0) None else s._1.headOption.map(a => (a, (s._1.drop(1), s._2 - 1))))

  def unfoldTakeWhile(p: A => Boolean): Stream[A] =
    Stream.unfold(this)(s => s.headOption.flatMap(a => if (p(a)) Some(a, s.drop(1)) else None))

  def zipWith[X >: A](s2: Stream[X])(f: (X, X) => X): Stream[X] =
    Stream.unfold((this, s2))(s => (s._1.headOption, s._2.headOption) match {
      case (Some(a1), Some(a2)) => Some((f(a1, a2), (s._1.drop(1), s._2.drop(1))))
      case _ => None
    })

  // Empty に対して drop してもエラーが起きずに そのまま Empty が返ってくるという前提
  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    Stream.unfold((this, s2))(s => (s._1.headOption, s._2.headOption) match {
      case (None, None) => None
      case (op1, op2) => Some((op1, op2), (s._1.drop(1), s._2.drop(1)))
    })

  // EX5.14
  // 検査対象のほうが短い分にはOK
  // あとは this が無限ストリームでも成立するかどうかがポイント？
  // ↓これだと無限ストリームに対応できなそう
  //  def startsWith[A](s: Stream[A]): Boolean =
  //    this.zipAll(s).forAll {
  //      case (Some(a1), Some(a2)) if a1 == a2 => true
  //      case (Some(_), None) => true
  //      case _ => false
  //    }

  // これいけるのでは
  // まずは zipAll その後 s 側の要素数 分だけ takeWhile
  // その後 残った Stream に対して要素同士を比較
  // this のほうが短かったりしたら自然とfalseになるよね
  // takeWhile してるから無限ストリームにも対応
  // 効率は知らん
  def startsWith[A](s: Stream[A]): Boolean =
    this.zipAll(s).takeWhile {
      case (_, Some(_)) => true
      case _ => false
    }.forAll {
      case (Some(a1), Some(a2)) if a1 == a2 => true
      case _ => false
    }

  // ex515
  def tails: Stream[Stream[A]] =
      Stream.unfold(this)(s => s.headOption.map(_ => (s, s.drop(1)))).append(Stream(Stream.empty))

  //    Stream.unfold(this){
  //      case c@Cons(h, t) => Some(c, t())
  //      case
  //    }


  // ex516
  // def tails = this.scanRight(this)(_)
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    this.foldRight((z, Stream(z))) {
      case (a, (b, s)) =>
        lazy val next = f(a, b)
        (next, Stream.cons(next, s))
    }._2

//  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
//    foldRight((z, Stream(z)))((a, p0) => {
//      // p0 is passed by-name and used in by-name args in f and cons. So use lazy val to ensure only one evaluation...
//      lazy val p1 = p0
//      val b2 = f(a, p1._1)
//      (b2, cons(b2, p1._2))
//    })._2

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


  // ex 5.8
  def constant[A](a: A): Stream[A] =
    Stream.cons(a, constant(a))

  // ex 5.9
  def from(n: Int): Stream[Int] =
    Stream.cons(n, from(n + 1))

  // ex 5.10
  def fibs: Stream[Int] = {
    def fib(prev: Int, next: Int): Stream[Int] =
      Stream.cons(prev, fib(next, prev + next))

    fib(0, 1)
  }

  // ex 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((a, s)) => Stream.cons(a, unfold(s)(f))
      case _ => Stream.empty[A]
    }

  // ex 5.12
  def unfoldFibs: Stream[Int] =
    unfold((0, 1))(pn => Some(pn._1, (pn._2, pn._1 + pn._2)))

  def unfoldFrom(n: Int): Stream[Int] =
    unfold(n)(i => Some(i, i + 1))

  def unfoldConstant(n: Int): Stream[Int] =
    unfold(n)(i => Some(i, i))

  def unfoldOnes: Stream[Int] =
    unfold(1)(_ => Some(1, 1))

}

