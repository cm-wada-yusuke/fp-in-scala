package chapter4.ex4_5

object Traverse {

  import chapter4.ex4_1._

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight(Some(List.empty[B]): Option[List[B]]) { (e: A, acc: Option[List[B]]) =>
      acc.flatMap(bs => f(e).map((fe: B) => fe :: bs)
      )
    }

}
