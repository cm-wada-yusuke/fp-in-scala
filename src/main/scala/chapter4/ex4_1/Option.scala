package chapter4.ex4_1

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    this.map(f).getOrElse(None)

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case None => default
  }

  // match 使っちゃった
  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case Some(_) => this
    case None => ob
  }

  // match 使っちゃった
//  def orElse2[B >: A](ob: => Option[B]): Option[B] =
//    this.flatMap(Some(_):Option[B]).getOrElse(ob)


  // パターンマッチ使っちゃった
  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
  }

  def filter2(f: A => Boolean): Option[A] = this.flatMap(a => if (f(a)) this else None)

}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

