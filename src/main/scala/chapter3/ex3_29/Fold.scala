package chapter3.ex3_29

import chapter3._

object Fold {

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    case Leaf(v) => f(v)
  }

  def foldSize[A](t: Tree[A]): Int =
    fold(t)(_ => 1)(1 + _ + _)

  def foldMaximum(t: Tree[Int]): Int =
    fold(t)(v => v)((l, r) => l max r)

  def foldDepth[A](t: Tree[A]): Int =
    fold(t)(_ => 0)((l, r) => 1 + (l max r))

  def foldMap[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(v => Leaf(f(v)): Tree[B])((l, r) => Branch(l, r))

}
