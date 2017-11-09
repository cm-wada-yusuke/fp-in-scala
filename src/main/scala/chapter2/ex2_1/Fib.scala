package chapter2.ex2_1

object Fib {

  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(t: Int, cur: Int, next: Int): Int =
      if (t <= 1) cur
      else if (t == 0) throw new RuntimeException("cant take zero value.")
      else go(t - 1, next, cur + next)
    go(n, 0, 1)
  }

}
