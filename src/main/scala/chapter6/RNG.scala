package chapter6

trait RNG {

  def nextInt: (Int, RNG)

}

case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

}

object EXRNG {

  // EX 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (i & Int.MaxValue, r) // 最上位ビット反転 = 符号反転
  }

  // EX 6.2
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i.toDouble / Int.MaxValue, r)
  }


  // EX6.3
  // RNGを伝搬させるのか、同じRNGを使いまわすのか迷った
  // 後続の関数と差を出すには伝搬させるのが妥当だろうという判断
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, ir) = nonNegativeInt(rng)
    val (d, dr) = double(ir)
    ((i, d), dr)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, dr) = double(rng)
    val (i, ir) = nonNegativeInt(dr)

    ((d, i), ir)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)

    ((d1, d2, d3), r3)
  }

  // EX6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    List.fill(count)(0).foldLeft((List.empty[Int], rng)) {
      case ((list, tr), _) =>
        val (ni, nr) = nonNegativeInt(tr)
        (list :+ ni, nr)
    }


}
