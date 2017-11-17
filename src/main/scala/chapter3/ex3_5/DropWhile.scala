package chapter3.ex3_5

import chapter3.ex3_2.Tail

object DropWhile {

  import chapter3._

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(a, t) if f(a) => dropWhile(t, f)
    case _ => l
  }

}
