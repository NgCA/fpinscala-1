package fpinscala.state

import fpinscala.state.RNG._
import org.scalacheck.Arbitrary
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class RNGTest extends AnyFunSuite with ScalaCheckPropertyChecks {

  test("Exercise 6.1 nonNegativeInt") {
    forAll(Arbitrary.arbitrary[Long]) {
      seed =>
        val simple = Simple(seed)
        val (rndInt, _) = nonNegativeInt(simple)
        assert(rndInt >= 0)
        assert(rndInt <= Int.MaxValue)
    }
  }

  test("Exercise 6.2 double returns Double between 0 and 1, not including 1") {
    forAll(Arbitrary.arbitrary[Long]) {
      seed =>
        val simple = Simple(seed)
        val (rndDbl, _) = double(simple)
        assert(rndDbl >= 0)
        assert(rndDbl < 1)
    }
  }

  test("Exercise 6.4 ints returns list of random ints") {
    val simple = Simple(1L)
    val (rndInts, nxtSimple) = ints(3)(simple)

    assert(rndInts.size == 3)
    assert(nxtSimple != simple)
  }

  test("Exercise 6.5 double returns Double between 0 and 1, not including 1") {
    forAll(Arbitrary.arbitrary[Long]) {
      seed =>
        val simple = Simple(seed)
        val (rndDbl, _) = doubleViaMap(simple)
        assert(rndDbl >= 0)
        assert(rndDbl < 1)
    }
  }

  test("Exercise 6.9 mapViaFlatMap") {
    val test = Simple(1)

    val map = RNG.map(RNG.int)(a => s"$a result")
    val mapViaFlatMap = RNG.mapViaFlatMap(RNG.int)(a => s"$a result")

    val mapped = map(test)
    val mappedViaFlatMap = mapViaFlatMap(test)

    assert(mapped == mappedViaFlatMap)
  }

  test("Exercise 6.9 map2ViaFlatMap") {
    val test = Simple(1)

    val map2 = RNG.map2(RNG.int, RNG.int)(_ + _)
    val map2ViaFlatMap = RNG.map2ViaFlatMap(RNG.int, RNG.int)(_ + _)

    val mapped = map2(test)
    val mapped2ViaFlatMap = map2ViaFlatMap(test)
    assert(mapped == mapped2ViaFlatMap)
  }
}
