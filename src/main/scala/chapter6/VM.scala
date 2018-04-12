package chapter6

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {


  //  def unlock(vm: VM, input: Input): VM = input match {
  //    case Turn => vm
  //    case Coin => vm.modify {
  //      case m@Machine(false, _, _) => m
  //      case Machine(true, ca, co) => Machine(false, ca, co + 1)
  //    }.get.map {
  //      case Machine(_, ca, co) => (ca, co)
  //    }


  //  def handle(vm: VM, input: Input): VM = input match {
  //    case Coin => vm
  //    case Turn => vm.modify {
  //      case m@Machine(true, _, _) => m
  //      case Machine(false, ca, co) => Machine(true, ca - 1, co)
  //    }.get.map {
  //      case Machine(_, ca, co) => (ca, co)
  //    }
  //  }

  //  def operation(vm: VM, input: Input): VM = vm.modify {
  //    case m@Machine(_, 0, _) => m
  //    case m => input match {
  //      case c@Coin => unlock(vm, c)
  //      case t@Turn => handle(vm, t)
  //    }
  //  }

}


object VM {

  import State._

  // 状態変化を定義
  type VM = Machine => Machine

  def unlock: VM = {
    case m@Machine(false, _, _) => m
    case Machine(true, ca, co) => Machine(locked = false, ca, co + 1)
  }

  def handle: VM = {
    case m@Machine(true, _, _) => m
    case Machine(false, ca, co) => Machine(locked = true, ca - 1, co)
  }

  def operation(input: Input): VM = (m: Machine) => (input, m) match {
    case (_, Machine(_, 0, _)) => m
    case (Coin, _) => unlock(m)
    case (Turn, _) => handle(m)
  }


  // 1. 入力がリストで、出力がStateなので、これを満たせそうなメソッドは sequence だけ。これを使う方向。
  // 2. sequence は出力として値リストが得られるが、これは使わなくて良い
  // 3. 出力の型を満たそうと思ったら、getで最後の状態を取得、そこから計算した値がそのまま欲しいスナックとコインの値になる。ここは問題ない。
  // 4. あとはsequenceの中身。sequenceの引数型に合わせる必要があるため、引数は List[State[Machine,なんでも良い]] とする必要がある。
  // 4-1. ということで inputs.map は確定。 Input => State[Machine,Unit] としたい。
  // 4-2. 何もないところから State を作る方法… get か set か modify。
  // 4-3. Input => set(doSomething(i)) でいける？
  // 4-4. できない。 Input を受け取って何をするかというと、「Machine をつくる」 ではなくて 「Machine を変化させる」 だから。
  // 4-5. つまり doSomething は Input => (Machine => Machine) というシグネチャになる。
  // 4-6. (Machine => Machine) から State は作れるか？ 作れる。 def modify[S](f: S => S): State[S, Unit] ここで勝利を確信する。
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs.map((i: Input) => modify[Machine](operation(i))))
    s <- get
  } yield (s.coins, s.candies)
}
