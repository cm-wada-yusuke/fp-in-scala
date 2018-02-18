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


}