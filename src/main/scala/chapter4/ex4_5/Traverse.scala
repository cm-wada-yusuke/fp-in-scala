package chapter4.ex4_5

import chapter4.ex4_1
import chapter4.ex4_3.Map2
import chapter4.ex4_3.Map2.map2

object Traverse {

  import chapter4.ex4_1._

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight(Some(List.empty[B]): Option[List[B]]) { (e: A, acc: Option[List[B]]) =>
      acc.flatMap(bs => f(e).map((fe: B) => fe :: bs)
      )
    }

  def traverse2[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight(ex4_1.Some(List.empty[B]): Option[List[B]]){
      (e: A, acc: Option[List[B]]) => map2(f(e), acc)(_ :: _)
    }

}
