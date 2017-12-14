package chapter3.ex3_29

import chapter3._

object Fold {

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(v) => f(v)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def foldSize[A](t: Tree[A]): Int =
    fold(t)(_ => 1)(1 + _ + _)

  def foldMaximum(t: Tree[Int]): Int =
    fold(t)(v => v)((l, r) => l max r)

  def foldDepth[A](t: Tree[A], s: A): Int = {
    def loop(acc: Tree[A], depth: Int): Int =
      fold(acc)(v => if (v == s) depth else 0)((l, r) => l max r)

    loop(t, 0)
  }

  def foldMap[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(v => Leaf(f(v)): Tree[B])((l, r) => Branch(l, r))

}
