package chapter3.ex3_24

import chapter3._

object HasSubSequence {

  // FIXME
  //     HasSubSequence.hasSubsequence(List(1, 2, 3, 1, 3), List(1, 3)) shouldBe true
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def loop(p: List[A], b: List[A]): Boolean = (p, b) match {
      case (Nil, Nil) => true
      case (Cons(x, y), Nil) => true
      case (Nil, Cons(x, y)) => false
      case (Cons(h1, acc1), Cons(h2, acc2)) if (h1 == h2) => true && loop(acc1, acc2)
      case (Cons(h1, acc1), Cons(h2, acc2)) if (h1 != h2) => false
    }

    (sup, sub) match {
      case (Nil, Nil) => true
      case (Cons(x, y), Nil) => true
      case (Nil, Cons(x, y)) => false
      case (Cons(h1, acc1), Cons(h2, acc2)) if (h1 == h2) => loop(acc1, acc2)
      case (Cons(h1, acc1), Cons(h2, acc2)) if (h1 != h2) => hasSubsequence(acc1, Cons(h2, acc2))
    }
  }

}
