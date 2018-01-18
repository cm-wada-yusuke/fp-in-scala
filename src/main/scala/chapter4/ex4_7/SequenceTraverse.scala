package chapter4.ex4_7

import chapter4.ex4_6._

object SequenceTraverse {

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    es.foldRight(Right(List.empty[A]): Either[E, List[A]])(_.map2(_)(_ :: _))

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldRight(Right(List.empty[B]): Either[E, List[B]])(f(_).map2(_)(_ :: _))


}
