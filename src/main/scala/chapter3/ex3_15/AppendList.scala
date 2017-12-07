package chapter3.ex3_15

import chapter3._
import chapter3.ex3_10.FoldLeft
import chapter3.ex3_14.Append

object AppendList {

  def appendList[A](ass: List[List[A]]): List[A] =
    FoldLeft.foldLeft[List[A], List[A]](ass, Nil)((acc, a) =>
      FoldLeft.foldLeft(a, acc)((bcc, b) => Append.append(bcc, b))
    )

}
