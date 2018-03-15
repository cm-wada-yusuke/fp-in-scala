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


  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  // EX6.5
  def mapDouble: Rand[Double] =
    map(nonNegativeInt)(_.toDouble / Int.MaxValue)

  // EX6.6
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  // EX6.7
  // はい気持ち良い〜
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldLeft(unit(List.empty[A]))(map2(_, _)(_ :+ _))
}
