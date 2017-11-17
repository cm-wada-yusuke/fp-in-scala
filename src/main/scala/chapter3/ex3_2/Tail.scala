package chapter3.ex3_2

object Tail {

  import chapter3._

  def tail[T](as: List[T]): List[T] = as match {
    case Cons(_, t) => t
    case Nil => Nil // シグネチャを満たすことを重視した
  }

}
