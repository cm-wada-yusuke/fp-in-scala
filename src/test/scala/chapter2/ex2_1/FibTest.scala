package chapter2.ex2_1

import org.scalatest.{FlatSpec, Matchers}

class FibTest extends FlatSpec with Matchers {

  "testFib" should "output"  in {
    Fib.fib(3) shouldEqual 1
  }

}
