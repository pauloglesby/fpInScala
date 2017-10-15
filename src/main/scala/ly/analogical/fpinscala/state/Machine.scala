package ly.analogical.fpinscala
package state

/**
  * Ex 6.11
  * Implement the following state machine
  */
case class Machine(locked: Boolean, candies: Int, coins: Int)

object Machine {
  import State._

  /**
    * Rules of machine:
    * 1. Inserting coin into locked machine causes it to unlock IF there's any candy left
    * 2. Turning knob on unlocked machine causes it to dispense candy then become locked
    * 3. Turning the knob on a locked machine does nothing
    * 4. Inserting a coin into an unlocked machine does nothing
    * 5. A machine that's out of candy ignores all inputs
    *
    * `simulateMachine` should return the current state of the machine, and a pair of (Int, Int) corresponding to
    * (candiesInMachine, coinsInMachine) at current state.
    */
  def update: Input => Machine => Machine = i => m =>
    (i, m) match {
      case (Coin, Machine(true, ca, co)) if ca > 0 => Machine(false, ca, co + 1)
      case (Turn, Machine(false, ca, co)) => Machine(true, ca - 1, co)
      case (Turn, Machine(true, _, _)) => m
      case (Coin, Machine(false, _, _)) => m
      case (Coin, Machine(_, 0, _)) => m
    }

  /**
    * The crucial perspective here is that `State[S, A]` is a type alias for a higher-order function!
    * No need to worry about "well where does the state come from?" - it doesn't!
    * The object returned by `simulateMachine` is a HOF that will only produce an answer when provided with a state (in this case, a `Machine`).
    * Functional imperative programming is confusing because we think at the level of *program*, yet the imperative style lures us into thinking
    * about injecting a state "at the top" and "following it down".
    * Functional imperative programming is powerful; `simulateMachine` here describes ALL POSSIBLE PROGRAMS that can be performed on ANY `Machine`
    * according to the rules given in `update`.
    */
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs.map(modify[Machine] _ compose update))
    s <- get
  } yield (s.coins, s.candies)

}

sealed trait Input
case object Coin extends Input
case object Turn extends Input


