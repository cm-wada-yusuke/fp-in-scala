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



    (n, n2, n3) shouldEqual (769497,1988230381,-1277571485)
    (nn, nn2, nn3) shouldEqual (769497,1988230381,869912163)

  }

}