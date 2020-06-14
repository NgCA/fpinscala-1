package fpinscala.laziness

class StreamTest extends org.scalatest.FunSuite {

  test("5.10 fibonnaci") {
    val fib = Stream.fibs()
    val actual = fib.take(6).toList
    val expected = List(0, 1, 1, 2, 3, 5)
    assert(actual == expected)
  }

  test("5.11 unfold") {
    val unfolded = Stream.unfold(0)(s => if (s % 5 == 0) Some(s + 5, s + 5) else None)
    val actual = unfolded.take(5).toList
    val expected = List(5, 10, 15, 20, 25)
    assert(actual == expected)
  }
}
