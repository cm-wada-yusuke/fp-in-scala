package chapter3.ex3_1


object Match {

  import chapter3._
  import List._

  def matchStatement: Int = {
    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(h, t) => h + sum(t)
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    }
    x
  }

}
