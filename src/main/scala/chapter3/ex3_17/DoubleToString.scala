package chapter3.ex3_17

import chapter3._
import chapter3.List._

object DoubleToString {

  def doubleToString(as: List[Double]): List[String] =
    foldRight[Double, List[String]](as, Nil)((a, acc) => Cons(a.toString, acc))


}
