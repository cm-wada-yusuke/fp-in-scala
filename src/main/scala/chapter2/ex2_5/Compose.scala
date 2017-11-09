package chapter2.ex2_5

object Compose {

  def compose[A, B, C](f: B => C, g: A => B): A => C = { a => f(g(a)) }

}
