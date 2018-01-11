package chapter4

import chapter4.ex4_4.Sequence
import org.scalatest.{FlatSpec, Matchers}


class Chapter4Test extends FlatSpec with Matchers {

  import chapter4.ex4_1._

  "ex41" should "output" in {
    val sut = Some(10)
    sut.map(_ * 10) shouldEqual Some(100)
    sut.flatMap(a => Some(a * 20)) shouldEqual Some(200)

    sut.getOrElse(3) shouldEqual 10
    None.getOrElse(3) shouldEqual 3

    sut.orElse(Some(20)) shouldEqual Some(10)
    None.orElse(Some(20)) shouldEqual Some(20)

    sut.filter(_ % 2 == 0) shouldEqual sut
    sut.filter(_ % 2 != 0) shouldEqual None
  }

  "ex44" should "output" in {
    val sut = List(Some(10), Some(11), Some(12))

    Sequence.sequence(sut) shouldEqual Some(List(10, 11, 12))

  }


}