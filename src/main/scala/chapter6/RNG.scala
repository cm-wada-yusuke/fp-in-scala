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
    // 1xxxxx & 01111111111111111111111111111
    // だめです　0xxxxx ^ 10000000000000000000000000000
  }

  // EX 6.2
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }


  // EX6.3
  // RNGを伝搬させるのか、同じRNGを使いまわすのか迷った
  // 後続の関数と差を出すには伝搬させるのが妥当だろうという判断
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, ir) = rng.nextInt
    val (d, dr) = double(ir)
    ((i, d), dr)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, dr) = double(rng)
    val (i, ir) = dr.nextInt

    ((d, i), ir)
  }

  def doubleInt2(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
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
        val (ni, nr) = tr.nextInt
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

  // INT.MAXが法で割り切れるならば等分できるのでランダム値がどこに来ても採用される確率は同じになる
  // 割り切れない時、INT.MAX から 法の最大倍数の間 に発生したランダム値は確率をゆがめる要因なので無視したい
  //
  // ランダム値から n 分だけ先に進めると マイナスになる ( i + n )
  // そこから ランダム値をn で割ったあまり mod だけ戻す - mod
  // この操作で INT.MAX に戻りきらず 負の値になる ならば ランダム値nが INT.MAX から 法の最大倍数の間にある
  def nonNegativeLessThan(n: Int): Rand[Int] = { rng =>
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if (i + (n - 1) - mod >= 0) (mod, rng2) else nonNegativeLessThan(n)(rng) // rng2 じゃないのこれ渡すの？
  }

  // EX6.8
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = { rng =>
    val (a, rng2) = f(rng)
    g(a)(rng2)
  }

  def nonNegativeLessThanByFlatMap(n: Int): Rand[Int] = flatMap(nonNegativeInt) {
    i =>
      rng =>
        val mod = i % n
        if (i + (n - 1) - mod >= 0) (mod, rng) else nonNegativeLessThanByFlatMap(n)(rng)
  }

  // EX6.9
  def mapByFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s) { a => rng => (f(a), rng) }

  def map2ByFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra) { a =>
      flatMap(rb) { b =>
        rng =>
          (f(a, b), rng)
      }
    }




}
