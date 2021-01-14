package fpinscala.laziness

import org.scalatest.funsuite.AnyFunSuite

class StreamTest extends AnyFunSuite {

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

  test("5.12 fibsViaUnfold") {
    val fibs = Stream.fibsViaUnfold()
    val actual = fibs.take(6).toList
    val expected = List(0, 1, 1, 2, 3, 5)
    assert(actual == expected)
  }

  test("5.12 fromViaUnfold") {
    val from = Stream.fromViaUnfold(1)
    val actual = from.take(4).toList
    val expected = List(1, 2, 3, 4)
    assert(actual == expected)
  }

  test("5.12 constantViaUnfold") {
    val constant = Stream.constantViaUnfold(3)
    val actual = constant.take(3).toList
    val expected = List(3, 3, 3)
    assert(actual == expected)
  }

  test("5.12 onesViaUnfold") {
    val ones = Stream.onesViaUnfold()
    val actual = ones.take(5).toList
    val expected = List(1, 1, 1, 1, 1)
    assert(actual == expected)
  }

  test("5.13 mapViaUnfold") {
    val a = Stream(0, 1, 2)
    val actual = a.map(_ + 1).toList
    val expected = List(1, 2, 3)
    assert(actual == expected)
  }

  test("5.13 takeViaUnfold") {
    val actualInfinite = Stream.ones.takeViaUnfold(3).toList
    val expected = List(1, 1, 1)
    assert(actualInfinite == expected)
    val actual = Stream(1, 1, 1, 1).takeViaUnfold(3).toList
    assert(actual == expected)
  }

  test("5.13 takeWhileViaUnfold") {
    val actual = Stream.from(1).takeWhileViaUnfold(_ % 5 != 0).toList
    val expected = List(1, 2, 3, 4)
    assert(actual == expected)
  }

  test("5.13 zipWithViaUnfold") {
    val first = Stream.from(1)
    val second = Stream.constant("a")
    val actual = first.zipWithViaUnfold(second)((f, s) => s"$s $f").take(3).toList
    val expected = List("a 1", "a 2", "a 3")
    assert(actual == expected)
  }

  test("5.13 zipAll") {
    val firstInfinite = Stream.from(1)
    val secondInfinite = Stream.constant("a")
    val actual = firstInfinite.zipAll(secondInfinite).take(2).toList
    val expected = List(
      (Some(1), Some("a")),
      (Some(2), Some("a"))
    )
    assert(actual == expected)
    val first = Stream(1, 2)
    val second = Stream("a")
    val actualUneven = first.zipAll(second).toList
    val expectedUneven = List(
      (Some(1), Some("a")),
      (Some(2), Option.empty[String])
    )
    assert(actualUneven == expectedUneven)
  }

  test("5.14 startsWith true") {
    val first = Stream(1, 2, 3)
    val second = Stream(1, 2)
    assert(first.startsWith(second))
  }

  test("5.14 starts should return false when first does not start with second") {
    val first = Stream(1, 2, 3)
    val second = Stream(1, 1)
    assert(!first.startsWith(second))
  }

  test("5.15 tails") {
    //scala> Stream(1,2,3) == Stream(1,2,3)
    //res4: Boolean = false
    //have to test this way since == does not look at elements of Stream
    val actual = Stream(1, 2, 3).tails.map(_.toList).toList
    val expected = List(
      List(1, 2, 3),
      List(2, 3),
      List(3),
      List()
    )
    assert(actual == expected)
  }

  test("5.16 scanRight") {
    val actual = Stream(1, 2, 3).scanRight(0)(_ + _).toList
    val expected = List(6,5,3,0)
    assert(actual == expected)
  }
}
