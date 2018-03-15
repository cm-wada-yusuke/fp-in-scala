package chapter6

import org.scalatest.{FlatSpec, Matchers}


class Chapter6Test extends FlatSpec with Matchers {

  import chapter6._

  "ex61" should "output" in {

    val sut = new SimpleRNG(2)

    val (n, rng) = sut.nextInt
    val (n2, rng2) = rng.nextInt
    val (n3, rng3) = rng2.nextInt

    val (nn, nrng) = EXRNG.nonNegativeInt(sut)
    val (nn2, nrng2) = EXRNG.nonNegativeInt(nrng)
    val (nn3, nrng3) = EXRNG.nonNegativeInt(nrng2)


    (n, n2, n3) shouldEqual(769497, 1988230381, -1277571485)
    (nn, nn2, nn3) shouldEqual(769497, 1988230381, 869912163)

  }

  "ex62" should "output" in {

    val sut = new SimpleRNG(2)

    val (n, rng) = sut.nextInt
    val (n2, rng2) = rng.nextInt
    val (n3, rng3) = rng2.nextInt

    val (nn, nrng) = EXRNG.double(sut)
    val (nn2, nrng2) = EXRNG.double(nrng)
    val (nn3, nrng3) = EXRNG.double(nrng2)


    (n, n2, n3) shouldEqual(769497, 1988230381, -1277571485)
    List(nn, nn2, nn3).map(_ * 10000).map(_.toInt) shouldEqual List(3, 9258, 4050)

  }

  "ex64" should "output" in {

    val sut = new SimpleRNG(2)

    val (ns, nrng) = EXRNG.ints(5)(sut)

    ns shouldEqual List(769497, 1988230381, 869912163, 319775261, 962104480)

  }

  "ex67" should "output" in {
    import EXRNG._
    val sut = List(unit(1), unit(2), unit(3))
    val simple = new SimpleRNG(2)


    sequence(sut)(simple)._1 shouldEqual List(1, 2, 3)
  }

}