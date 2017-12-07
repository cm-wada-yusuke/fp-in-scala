package chapter3.ex3_10

import chapter3.List
import chapter3.Nil
import chapter3.Cons

object FoldLeft {

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }


}
