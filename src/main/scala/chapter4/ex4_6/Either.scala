package chapter4.ex4_6

sealed trait Either[+E, +A] {

  def map[B](f: A => B): Either[E, B] = this match {
    case Right(r) => Right(f(r))
    case l@Left(_) => l
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(r) => f(r)
    case l@Left(_) => l
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case r@Right(_) => r
    case _ => b
  }

  // どっちもLeftのときにどっちのLeftを返すか問題
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = (this, b) match {
    case (Right(a), Right(b)) => Right(f(a, b))
    case (Right(_), lb@Left(_)) => lb
    case (la@Left(_), _) => la
  }

}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

