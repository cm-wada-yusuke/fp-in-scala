package chapter3.ex3_14

import chapter3._
import chapter3.List._
import chapter3.ex3_10.FoldLeft

object Append {

  // 初っ端の要素をNilでなくリストにしてやればそれが末尾にくっつく
  def append[A](as: List[A], a: A): List[A] =
    foldRight[A, List[A]](as, List(a))((a, acc) => Cons(a, acc))

}
