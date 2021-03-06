package chapter2.ex2_4

object Uncurry {

  // braceいらない
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = { (a, b) => f(a)(b) }

}
