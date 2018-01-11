package chapter4.ex4_4

import chapter4.ex4_1
import chapter4.ex4_3.Map2

object Sequence {

  import chapter4.ex4_1._

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight(ex4_1.Some(List.empty[A]): Option[List[A]])(Map2.map2(_, _)(_ :: _))


}
