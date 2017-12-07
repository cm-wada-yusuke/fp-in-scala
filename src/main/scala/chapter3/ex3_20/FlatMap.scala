package chapter3.ex3_20

import chapter3._
import chapter3.ex3_10.FoldLeft
import chapter3.ex3_15.AppendList

object FlatMap {

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    FoldLeft.foldLeft[A, List[B]](as, Nil)((acc, a) => AppendList.appendList(List(acc, f(a))))

}
