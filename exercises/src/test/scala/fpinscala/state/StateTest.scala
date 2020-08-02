package fpinscala.state

import fpinscala.state.RNG.{Rand, Simple}
import org.scalatest.BeforeAndAfter

class StateTest extends org.scalatest.FunSuite with BeforeAndAfter {

  val simple: Simple = Simple(1)
  val state = State[RNG, Int](rng => rng.nextInt)
  val (nextRand, nextRng) = state.run(simple)

  test("Exercise 6.10 unit") {
    val unit = State.unit[RNG, Int](123)

    val actual = unit.run(simple)
    val expected = (123, simple)
    assert(actual == expected)
  }

  test("Exercise 6.10 map") {
    val map = state.map(a => s"mapped $a")

    val actual = map.run(simple)
    val expected = (s"mapped $nextRand", nextRng)
    assert(actual == expected)
  }

  test("Exercise 6.10 map2") {
    val map2 = state.map2(State.unit[RNG, Int](1))((a, b) => s"$a with $b")

    val actual = map2.run(simple)
    val expected = (s"$nextRand with 1", nextRng)
    assert(actual == expected)
  }

  test("6.10 flatMap") {
    val flatMap = state.flatMap(i => State.unit[RNG, String](s"flatMapped $i"))

    val actual = flatMap.run(simple)
    val expected = (s"flatMapped $nextRand", nextRng)
    assert(actual == expected)
  }

  test("6.10 sequence") {
    val stateActions: List[State[RNG, Int]] = scala.collection.immutable.List(
      State(RNG.int)
    )
    val seq = State.sequence[RNG, Int](stateActions)
    val actual = seq.run(simple)
    val expected = (List(nextRand), nextRng)
    assert(actual == expected)
  }

  test("6.11 insertCoin - Inserting a coin into a locked machine will cause it to unlock if there's" +
    " any candy left") {
    val machine = Machine(true, 1, 1)

    val actual = State.insertCoin(machine)
    val expected = Machine(false, 1, 2)

    assert(actual == expected)
  }

  test("6.11 insertCoin - If the machine is unlocked then the state of the machine is returned") {
    val machine = Machine(false, 1, 1)

    val actual = State.insertCoin(machine)
    assert(actual == machine)
  }

  test("6.11 insertCoin - If the machine locked and is out of candies then the state of the machine is returned") {
    val machine = Machine(true, 0, 1)

    val actual = State.insertCoin(machine)
    assert(actual == machine)
  }

  test("6.11 turnKnob - Turning the knob on an unlocked machine will cause it to dispense candy and become locked.") {
    val machine = Machine(false, 1, 1)

    val actual = State.turnKnob(machine)
    val expected = Machine(true, 0, 1)

    assert(actual == expected)
  }

  test("6.11 turnKnob - If the machine is locked then the state of machine is returned") {
    val machine = Machine(true, 1, 1)

    val actual = State.turnKnob(machine)
    assert(actual == machine)
  }

  test("6.11 turnKnob - if the machine is unlocked, but out of candies then the state of machine is returned") {
    val machine = Machine(false, 0, 1)

    val actual = State.turnKnob(machine)
    assert(actual == machine)
  }

}
