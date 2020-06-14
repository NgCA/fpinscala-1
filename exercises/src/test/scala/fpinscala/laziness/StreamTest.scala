package fpinscala.laziness

class StreamTest extends org.scalatest.FunSuite {

  test("5.10 fibonnaci") {
    val fib = Stream.fibs()
    val actual = fib.take(6).toList
    val expected = List(0,1,1,2,3,5)
    assert(actual == expected)
  }
}
