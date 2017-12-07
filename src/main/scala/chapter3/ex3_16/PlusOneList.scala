package chapter3.ex3_16

import chapter3._
import chapter3.List._

object PlusOneList {

  def plusOneList(as: List[Int]): List[Int] =
    foldRight[Int, List[Int]](as, Nil)((a, acc) => Cons(a + 1, acc))
}
