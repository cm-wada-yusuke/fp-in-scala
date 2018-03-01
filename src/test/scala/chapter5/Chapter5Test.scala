package chapter5

import org.scalatest.{FlatSpec, Matchers}


class Chapter5Test extends FlatSpec with Matchers {

  import chapter5._

  "ex51" should "output" in {
    val sut = Stream(1, 2, 3, 4, 5)

    sut.toList shouldEqual List(1, 2, 3, 4, 5)
  }

  "ex52" should "output" in {
    val sut = Stream(1, 2, 3, 4, 5)

    sut.take(3).toList shouldEqual Stream(1, 2, 3).toList
    sut.take(1).toList shouldEqual Stream(1).toList
    sut.drop(3).toList shouldEqual Stream(4, 5).toList
  }

  "ex53" should "output" in {
    val sut = Stream(1, 2, 3, 4, 5)

    sut.takeWhile(_ < 5).toList shouldEqual Stream(1, 2, 3, 4).toList
  }

  "ex54" should "output" in {
    val sut = Stream(1, 2, 3, 4, 5)

    sut.forAll(_ < 6) shouldBe true
    sut.forAll(_ < 5) shouldBe false
  }

  "ex55" should "output" in {
    val sut = Stream(1, 2, 3, 4, 5)

    sut.takeWhileByFoldRight(_ < 5).toList shouldEqual Stream(1, 2, 3, 4).toList
  }

  "ex56" should "output" in {
    val sut = Stream(1, 2, 3, 4, 5)

    sut.headOptionByFoldRight shouldEqual Some(1)
  }

  "ex57" should "output" in {
    val sut = Stream(1, 2, 3, 4, 5)

    sut.map(_ * 2).toList shouldEqual List(2, 4, 6, 8, 10)
    sut.filter(_ % 2 == 0).toList shouldEqual List(2, 4)
    sut.append(Stream(10, 11)).toList shouldEqual List(1, 2, 3, 4, 5, 10, 11)
    sut.flatMap(i => Stream(i * 3)).toList shouldEqual List(3, 6, 9, 12, 15)
  }

  "ex58" should "output" in {
    Stream.constant(1).take(5).toList shouldEqual List(1, 1, 1, 1, 1)
  }

  "ex59" should "output" in {
    Stream.from(0).take(3).toList shouldEqual List(0, 1, 2)
  }
  "ex510" should "output" in {
    Stream.fibs.take(10).toList shouldEqual List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
  }

  "ex511" should "output" in {
    Stream.unfold(0)((i: Int) => Some(i, i + 1)).take(5).toList shouldEqual List(0, 1, 2, 3, 4)
  }

  "ex512" should "output" in {
    Stream.unfoldFibs.take(11).toList shouldEqual List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55)
    Stream.unfoldFrom(5).take(5).toList shouldEqual List(5, 6, 7, 8, 9)
    Stream.unfoldConstant(10).take(3).toList shouldEqual List(10, 10, 10)
    Stream.unfoldOnes.take(2).toList shouldEqual List(1, 1)
  }

  "ex513" should "output" in {
    val sut = Stream(1, 2, 3, 4, 5)
    val sut2 = Stream(10, 20, 30, 40, 50)
    val sut3 = Stream(10)

    sut.unfoldMap(_ * 10).toList shouldEqual List(10, 20, 30, 40, 50)
    sut.unfoldTake(4).toList shouldEqual List(1, 2, 3, 4)
    sut.unfoldTakeWhile(_ < 4).toList shouldEqual List(1, 2, 3)
    sut.zipWith(sut2)(_ + _).toList shouldEqual List(11, 22, 33, 44, 55)
    sut.zipAll(sut2).toList shouldEqual List((Some(1), Some(10)), (Some(2), Some(20)), (Some(3), Some(30)), (Some(4), Some(40)), (Some(5), Some(50)))
    sut.zipAll(sut3).toList shouldEqual List((Some(1), Some(10)), (Some(2), None), (Some(3), None), (Some(4), None), (Some(5), None))

  }

}