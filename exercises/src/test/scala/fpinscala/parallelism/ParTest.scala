package fpinscala.parallelism

import org.scalatest.funsuite.AnyFunSuite

import java.util.concurrent.{Executors, TimeUnit, TimeoutException}

class ParTest extends AnyFunSuite {

  val es = Executors.newFixedThreadPool(2)

  test("map2") {
    val firstPar = Par.unit(1)
    val secondPar = Par.unit(2)

    val actual = Par.map2(firstPar, secondPar)(_ + _)
    val expected = 3
    assert(actual(es).get == expected)
  }

  test("map2WithTimeout") {
    val firstPar = Par.unit(1)
    val secondPar = Par.unit(2)
    val actual = Par.map2WithTimeout(firstPar, secondPar, 0, TimeUnit.MILLISECONDS)(_ + _)
    assertThrows[TimeoutException](actual(es).get)
  }

}
