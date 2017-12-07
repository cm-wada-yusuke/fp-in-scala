package chapter3.ex3_19

import chapter3.List._
import chapter3._

object Filter {

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight[A, List[A]](as, Nil)((a, acc) => if (f(a)) Cons(a, acc) else acc)

}
