package fpinscala.gettingstarted

class MyModuleTest extends org.scalatest.FunSuite {

  test("Exercise 2.1 fibonacci"){
    //example fibonacci sequence 0,1,1,2,3,5,8
    assert(MyModule.fib(0) == 0)
    assert(MyModule.fib(1) == 1)
    assert(MyModule.fib(6) == 8)
  }
}
