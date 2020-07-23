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
}
