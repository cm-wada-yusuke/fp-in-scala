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


  // あんまりカタルシスがない…絶対他にやり方ある…
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs.map((i: Input) => modify[Machine](operation(i)))) // パズルしてたらたまたまできた…ごめんなさいよくわかってない
    s <- get
  } yield (s.coins, s.candies)
}
