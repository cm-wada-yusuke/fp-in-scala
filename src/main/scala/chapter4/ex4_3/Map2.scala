package chapter4.ex4_3

object Map2 {

  import chapter4.ex4_1._

  // for-complehension 案件
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(a =>
      b.map(b =>
        f(a, b)
      )
    )

}
