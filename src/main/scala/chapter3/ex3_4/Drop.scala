package chapter3.ex3_4

object Drop {

  import chapter3._

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(_, _) if n <= 0 => l
    case Cons(_, t) if n > 0 => drop(t, n - 1)
  }

}
