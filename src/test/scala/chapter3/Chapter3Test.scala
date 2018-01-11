package chapter3

import chapter3.ex3_1.Match
import chapter3.ex3_15.AppendList
import chapter3.ex3_16.PlusOneList
import chapter3.ex3_17.DoubleToString
import chapter3.ex3_2.Tail
import chapter3.ex3_20.FlatMap
import chapter3.ex3_21.FlatMapFilter
import chapter3.ex3_3.SetHead
import chapter3.ex3_4.Drop
import chapter3.ex3_5.DropWhile
import chapter3.ex3_22.PlusEachElements
import chapter3.ex3_23.ZipWith
import chapter3.ex3_24.HasSubSequence
import chapter3.ex3_29.Fold
import org.scalatest.{FlatSpec, Matchers}

class Chapter3Test extends FlatSpec with Matchers {

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
    DropWhile.dropWhile(List(1, 2, 3, 4, 5), (a: Int) => a < 5) shouldEqual List(5)
  }

  "ex315" should "output" in {
    AppendList.appendList(List(List(1, 2), List(2, 3), List(3), List(4, 5, 6, 7))) shouldEqual List(1, 2, 2, 3, 3, 4, 5, 6, 7)
  }

  "ex316" should "output" in {
    PlusOneList.plusOneList(List(1, 2, 3, 4, 5)) shouldEqual List(2, 3, 4, 5, 6)
  }

  "ex317" should "output" in {
    DoubleToString.doubleToString(List(1.1, 2.2, 3.3, 4.4, 5.5)) shouldEqual List("1.1", "2.2", "3.3", "4.4", "5.5")
  }

  "ex320" should "output" in {
    FlatMap.flatMap(List(1, 2, 3))(i => List(i, i)) shouldEqual List(1, 1, 2, 2, 3, 3)
  }

  "ex321" should "output" in {
    FlatMapFilter.filter(List(1, 2, 3, 4, 5))(_ % 2 == 0) shouldEqual List(2, 4)
  }

  "ex322" should "output" in {
    PlusEachElements.plusEachElements(List(1, 2, 3), List(4, 5, 6)) shouldEqual List(5, 7, 9)
  }

  "ex323" should "output" in {
    ZipWith.zipWith(List(1, 2, 3), List(4, 5, 6))(_ * _) shouldEqual List(4, 10, 18)
  }

  "ex324" should "output" in {
    HasSubSequence.hasSubsequence(List(1, 2, 3), List(4, 5, 6)) shouldBe false
    HasSubSequence.hasSubsequence(List(1, 2, 3), List(2, 3)) shouldBe true
//    HasSubSequence.hasSubsequence(List(1, 2, 3, 1, 3), List(1, 3)) shouldBe true ---- !!!
  }

  "ex329" should "output" in {
    val st = Branch(
      Branch(Leaf("a"), Leaf("b")),
      Branch(Leaf("c"), Leaf("d"))
    )
    val it = Branch(
      Branch(Leaf(255), Leaf(0)),
      Branch(Leaf(300), Leaf(-10))
    )

    val it2 = Branch(
      Branch(
        Branch(Leaf(255), Leaf(0)),
        Branch(Leaf(300), Leaf(0))
      ),
      Branch(Leaf(300), Leaf(-10))
    )

    Fold.foldSize(st) shouldEqual 7
    Fold.foldMaximum(it) shouldEqual 300
    Fold.foldDepth(it) shouldEqual 2
    Fold.foldDepth(it2) shouldEqual 3
    Fold.foldMaximum(Fold.foldMap(it2)(_ - 10)) shouldEqual 290

  }




}