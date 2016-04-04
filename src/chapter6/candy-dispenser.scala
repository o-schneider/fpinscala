package chapter6

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Machine {
  def activate(input: Input): State[Machine, (Int, Int)] = State(m => {
    input match {
      case Coin if m.locked =>
        val coins = m.coins + 1
        ((coins, m.candies), Machine(false, m.candies, coins))
      case Turn if !m.locked =>
        val candies = m.candies - 1
        ((m.coins, candies), Machine(true, candies, m.coins))
      case _ => ((m.coins, m.candies), m)
    }
  })

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    val state: State[Machine, List[(Int, Int)]] = State.sequence(inputs.map(input => activate(input)))
    val map: State[Machine, (Int, Int)] = state.map(l => l.last)
    map
  }
}

object MachineTest {

  def main(args: Array[String]) {
    val machine = Machine(true, 5, 10)
    println("Machine.simulateMachine(List(Coin, Turn)) = " + Machine.simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)).run(machine))
  }
}