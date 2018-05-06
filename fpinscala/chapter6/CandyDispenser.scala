sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean = true, candies: Int = 100, coins: Int = 0)

/* The rules of the machine:
* - inserting a coin into a locked machine will cause it to unlock if there's any candy left.
* - turning the knob on an unlocked machine will cause it to dispense a candy and become locked.
* - turning the knob on a locked machine or inserting a coin into an unlocked machine does nothing.
* - a machine that's out of candy ignores all input.
* */

object CandyDispenser {

  def newMachineState = (input: Input) => (machine: Machine) => (input, machine) match {
    case (_, Machine(_, 0, _)) => machine
    case (Coin, Machine(false, _, _)) => machine
    case (Turn, Machine(true, _, _)) => machine
    case (Coin, Machine(true, _, _)) => Machine(false, machine.candies, machine.coins + 1)
    case (Turn, Machine(false, _, _)) => Machine(true, machine.candies - 1, machine.coins)
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    for {
      _ <- State.sequence(inputs.map(State.modify[Machine] _ compose newMachineState)) // set
      s <- State.get
    } yield (s.coins, s.candies)
  }
}
