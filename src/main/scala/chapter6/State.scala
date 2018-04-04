package chapter6

case class State[S, +A](run: S => (A, S)) {

  // EX6.10
  def unit[X](a: X): State[S, X] = State(s => (a, s))

  def map[B](f: A => B): State[S, B] = State { s =>
    val (a, s2) = this.run(s)
    (f(a), s2)
  }

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = State { s =>
    val (a, s2) = this.run(s)
    val (b, s3) = sb.run(s2)
    (f(a, b), s3)
  }

  def flatMap[B](g: A => State[S, B]): State[S, B] = State { s =>
    val (a, s2) = this.run(s)
    g(a).run(s2)
  }

  // コンパニオンオブジェクトに書いたら Rand みたいにきれいにかけるかも
  def sequence[X >: A](fs: List[State[S, X]]): State[S, List[X]] =
    fs.foldLeft(unit(List.empty[X])) { (bcc, sa) =>
      bcc.map2(sa)(_ :+ _)
    }

}

object State {
  type Rand[A] = State[RNG, A]

}


