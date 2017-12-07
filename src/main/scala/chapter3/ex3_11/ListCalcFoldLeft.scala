package chapter3.ex3_11

import chapter3._
import chapter3.ex3_10.FoldLeft._

object ListCalcFoldLeft {

  def sum(as: List[Int]): Int =
    foldLeft(as, 0)(_ + _)

}
