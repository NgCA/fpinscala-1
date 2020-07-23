package fpinscala.gettingstarted

class GettingStartedTest extends org.scalatest.FunSuite {

  test("Exercise 2.1 fibonacci") {
    //example fibonacci sequence 0,1,1,2,3,5,8
    assert(MyModule.fib(0) == 0)
    assert(MyModule.fib(1) == 1)
    assert(MyModule.fib(6) == 8)
  }

  test("Exercise 2.2 isSorted returns true when sorted") {
    val sorted = Array(1, 2, 3)
    assert(PolymorphicFunctions.isSorted(sorted, (a: Int, b: Int) => a < b))
  }

  test("Exercise 2.2 isSorted returns false when not sorted") {
    val notSorted = Array(1, 2, 1)
    assert(!PolymorphicFunctions.isSorted(notSorted, (a: Int, b: Int) => a < b))
  }

  test("Exercise 2.3 curry") {
    def func = (a: Int, b: Int) => a + b

    val curriedFunc = PolymorphicFunctions.curry(func)
    assert(func(1, 1) == curriedFunc(1)(1))
  }

  test("Exercise 2.4 uncurry") {
    def curried = (a: Int) => (b: Int) => a + b

    val uncurried = PolymorphicFunctions.uncurry(curried)
    assert(curried(1)(1) == uncurried(1, 1))
  }

  test("exercise 2.5 compose") {
    def f = (a: Int) => s"abc $a"

    def g = (a: Int) => a + 1

    val fComposeG = PolymorphicFunctions.compose(f, g)
    assert(fComposeG(1) == "abc 2")
  }
}
