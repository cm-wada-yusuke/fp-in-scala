package chapter2.ex2_2

object Sorted {

  /**
    *
    * @param as      検査対象の配列。
    * @param ordered 2値をとりソート条件を満たしているかどうかを返す関数。
    * @tparam A 検査対象配列の要素型
    * @return asがソート済かどうか
    */
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def loop(n: Int): Boolean =
      if (n >= as.length - 1) ordered(as(n - 1), as(n)) // 終わり
      else if (as.length <= 1) true //要素がひとつ以下のときは常にtrue
      else if (ordered(as(n - 1), as(n))) loop(n + 1)
      else false

    loop(1)
  }
}
