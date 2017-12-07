package chapter3.ex3_22

import chapter3._

object PlusEachElements {

  def plusEachElements(as: List[Int], bs: List[Int]): List[Int] =
    (as, bs) match {
      case (Nil, Nil) => Nil
      case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, plusEachElements(xs, ys))
      case (a@Cons(_, _), Nil) => a
      case (Nil, b@Cons(_, _)) => b
    }

}
