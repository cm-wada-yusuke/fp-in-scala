package chapter3.ex3_1

import chapter3.ex3_2.Tail
import chapter3.ex3_3.SetHead
import chapter3.ex3_4.Drop
import chapter3.ex3_5.DropWhile
import org.scalatest.{FlatSpec, Matchers}

class Chapter3Test extends FlatSpec with Matchers {

  import chapter3.List

  "ex31" should "output" in {
    Match.matchStatement shouldEqual 15
  }

  "ex32" should "output" in {
    Tail.tail(List(1, 2, 3, 4, 5)) shouldEqual List(2, 3, 4, 5)
  }

  "ex33" should "output" in {
    SetHead.setHead(6, List(1, 2, 3, 4, 5)) shouldEqual List(6, 2, 3, 4, 5)
    SetHead.setHead2(6, List(1, 2, 3, 4, 5)) shouldEqual List(6, 2, 3, 4, 5)
  }

  "ex34" should "output" in {
    Drop.drop(List(1, 2, 3, 4, 5), 3) shouldEqual List(4, 5)
    Drop.drop(List(1, 2, 3, 4, 5), 100) shouldEqual List()
  }

  "ex35" should "output" in {
    DropWhile.dropWhile(List(1, 2, 3, 4, 5), (a:Int) => a < 5) shouldEqual List(5)
  }

}