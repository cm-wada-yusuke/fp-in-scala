package chapter4

import java.awt.image.RGBImageFilter

import chapter4.ex4_4.Sequence
import chapter4.ex4_5.Traverse
import chapter4.ex4_7.SequenceTraverse
import org.scalatest.{FlatSpec, Matchers}


class Chapter4Test extends FlatSpec with Matchers {

  import chapter4.ex4_1._
  import chapter4.ex4_6._

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

  "ex45" should "output" in {
    val sut = List(10, 20, 25)
    val sut2 = List(10, 0, 25)

    val f = (y: Int) => {
      if (y == 0) None else Some(100 / y)
    }

    Traverse.traverse(sut)(f) shouldEqual Some(List(10, 5, 4))
    Traverse.traverse(sut2)(f) shouldEqual None

  }

  "ex47 sequence" should "output" in {
    val sut = List(Right(10), Right(20), Right(25))
    val sut2 = List(Right(10), Left(0), Right(20))

    SequenceTraverse.sequence(sut) shouldEqual Right(List(10, 20, 25))
    SequenceTraverse.sequence(sut2) shouldEqual Left(0)

  }

  "ex47 traverse" should "output" in {
    val sut = List(10, 20, 25)
    val sut2 = List(10, 0, 20)

    val f: Int => Either[String, Int] = (y: Int) =>
      if (y == 0) Left("zero divide exception😍") else Right(100 / y)


    SequenceTraverse.traverse(sut)(f) shouldEqual Right(List(10, 5, 4))
    SequenceTraverse.traverse(sut2)(f) shouldEqual Left("zero divide exception😍")

  }


}