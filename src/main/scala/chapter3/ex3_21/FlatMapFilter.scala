package chapter3.ex3_21

import chapter3._
import chapter3.ex3_20.FlatMap

object FlatMapFilter {

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    FlatMap.flatMap[A, A](as)(a => if(f(a)) List(a) else Nil)

}
