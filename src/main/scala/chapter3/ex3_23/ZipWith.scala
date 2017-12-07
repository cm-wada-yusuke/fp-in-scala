package chapter3.ex3_23

import chapter3._
import chapter3.List._

object ZipWith {

  // 誰か末尾再帰にしてくれたのむ！！
  def zipWith[A](as: List[A], bs: List[A])(f: (A, A) => A): List[A] =
    (as, bs) match {
      case (Nil, Nil) => Nil
      case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
      case (a@Cons(_, _), Nil) => a
      case (Nil, b@Cons(_, _)) => b
    }


}
