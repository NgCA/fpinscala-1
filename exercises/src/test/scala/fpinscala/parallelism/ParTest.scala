package fpinscala.parallelism

import fpinscala.parallelism.Par.Par
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

  test("sequence") {
    val input: List[Par[Int]] = List(
      Par.unit(1),
      Par.unit(2)
    )

    val actual: Par[List[Int]] = Par.sequence(input)
    val expected = List(1,2)
    assert(actual(es).get == expected)
  }

  //here to show why I had to call get for sequence test's assert
  test("equality of functions") {
    //equality of functions does not check equality of implementation
    val a = () => 1
    val b = () => 1
    assert(a != b)
  }

}
