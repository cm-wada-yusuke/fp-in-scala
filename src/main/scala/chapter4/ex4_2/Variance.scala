package chapter4.ex4_2

object Variance {

  def variance(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
      .flatMap(a => Some(xs.map(e => Math.pow(e - a, 2))))
      .flatMap(as => Some(as.sum / as.length))


}


