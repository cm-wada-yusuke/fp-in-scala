package chapter2.ex2_3

object Curry {

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = { a => f(a, _) }

}
