package chapter7

import java.util.concurrent.TimeUnit

abstract class ExecutorService {
  def submit[A](a: Callable[A]): Future[A]
}

trait Callable[A] {
  def call: A
}

trait Future[A] {
  def get: A

  def get(timeout: Long, unit: TimeUnit): A

  def cancel(evenIfRunning: Boolean): Boolean

  def isDone: Boolean

  def isCancelled: Boolean
}


object PAR {

  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def get(timeout: Long, units: TimeUnit) = get

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = (es: ExecutorService) => {
    val af = a(es)
    val bf = b(es)
    UnitFuture(f(af.get, bf.get))
  }

  def fork[A](a: => Par[A]): Par[A] = es => es.submit(new Callable[A] {
    def call = a(es).get
  })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))


  //EX7.3
  private case class TimeoutFuture[A](a: A) extends Future[(A, Long)] {

    def get: (A, Long) = {
      val start = System.currentTimeMillis()
      a
    }

    def get(timeout: Long, unit: TimeUnit): (A, Long) = get

    def isDone = true

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }

}
