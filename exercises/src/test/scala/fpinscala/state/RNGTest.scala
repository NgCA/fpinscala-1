package fpinscala.state

import fpinscala.state.RNG.Simple

class RNGTest extends org.scalatest.FunSuite {

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
