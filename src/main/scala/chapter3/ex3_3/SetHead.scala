package chapter3.ex3_3

import chapter3.ex3_2.Tail

object SetHead {

  import chapter3._

  def setHead[A](a: A, as:List[A]): List[A] = as match {
    case Cons(_, t) => Cons(a, t)
    case Nil => Cons(a, Nil)
  }

  def setHead2[A](a: A, as:List[A]): List[A] = Cons(a, Tail.tail(as))

}
