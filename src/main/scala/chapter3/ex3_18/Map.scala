package chapter3.ex3_18

import chapter3._
import chapter3.List._

object Map {

  def map[A, B](as: List[A])(f: A => B): List[B] =
    foldRight[A, List[B]](as, Nil)((a, acc) => Cons(f(a), acc))

}
