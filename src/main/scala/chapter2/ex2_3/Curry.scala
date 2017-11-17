package chapter2.ex2_3

object Curry {

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = (b: B) => f(a, b)

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = (a: A) => f(a, _)

  def curry2[A, B, C](f: (A, B) => C): A => (B => C) = (a: A) => (b: B) => f(a, b)


}
