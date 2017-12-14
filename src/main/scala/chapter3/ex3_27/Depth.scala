package chapter3.ex3_27

import chapter3._

object Depth {

  def depth[A](t: Tree[A], s: A): Int = {
    def loop(acc: Tree[A], depth: Int): Int = acc match {
      case Leaf(v) if s == v => depth
      case Leaf(v) => 0
      case Branch(l, r) => loop(l, depth + 1) max loop(r, depth + 1)
    }
    loop(t, 0)
  }

}
