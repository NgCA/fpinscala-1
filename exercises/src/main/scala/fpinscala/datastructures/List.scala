package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = {
    //Exercise 3.14
    /*a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }*/
    foldRight(a1, a2)((h, l) => Cons(h, l))
    /*
    essentially the fold will iterate through the list and then add the second list at the end
    foldRight will keep the order of the first list
     */
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  //Exercise 3.11 Write sum, product, and a function to compute the length of a list using foldLeft
  def sum2(ns: List[Int]): Int = {
    //foldRight(ns, 0)((x, y) => x + y)
    foldLeft(ns, 0)(_ + _)
  }

  def product2(ns: List[Double]): Double =
  //foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar
    foldLeft(ns, 1.0)(_ * _)

  def length2[A](ns: List[A]) = {
    foldLeft(ns, 0)((acc, _) => acc + 1)
  }

  //Exercise 3.2
  def tail[A](l: List[A]): List[A] = {
    l match {
      //from solution can use sys.error instead of returning Nil for error handling
      case Nil => Nil
      case Cons(_, xs) => xs
    }
  }

  //Exercise 3.3
  def setHead[A](l: List[A], h: A): List[A] = {
    l match {
      case Nil => sys.error("setHead on empty list")
      case Cons(_, t) => Cons(h, t)
    }
  }

  //Exercise 3.4
  //my first try was calling tail, misunderstood meaning of "generalize" in problem description
  //"generalize" => reuse implementation pattern of tail, not call it
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else
      l match {
        case Nil => Nil
        case Cons(_, xs) => drop(xs, n - 1)
      }
  }

  //Exercise 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    //my solution
    /*l match{
      case Nil => Nil
      case Cons(h,t) => if (f(h)) dropWhile(t, f) else l
    }*/

    //solution from book shows that the conditional in my second case can be moved to before the =>
    l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => l
    }
  }

  //Exercise 3.6
  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  }

  //Exercise 3.9
  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((_, acc) => acc + 1)
  }

  //Exercise 3.10
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil => z
      //result must be calculated from f and then passed in through z
      //followed types and saw that f returns same type as Z so know that I have to use f to replace value of z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }
  /*
  Example:
    foldLeft( List(1,2,3), 0)(_ + _)
    foldLeft( List(2,3), f(0, 1)(_ + _)
    foldLeft( List(2,3), 1)(_ + _)
    foldLeft( List(3), f(1, 2)(_ + _)
    foldLeft( List(3), 3)(_ + _)
    foldLeft( Nil, f(3,3))(_ + _)
    foldLeft( Nil, 6)(_ + _)
    6
     */

  //Exercise 3.12
  //Write a function that returns the reverse of a list (given List(1,2,3) it returns List(3,2,1)). See if you can write it using a fold.
  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List[A]())((reversedList, x) => Cons(x, reversedList))

  /*
  Exercise 3.13
  Hard: Can you write foldLeft in terms of foldRight? How about the other way around?
   Implementing foldRight via foldLeft is useful because it lets us implement foldRight tail-recursively,
   which means it works even for large lists without overflowing the stack.
   */
  def foldRightWithLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
    //incorrect foldLeft(l, z)((z, l) => f(l, z))
    //This is from the answer key
    //foldRight evaluates from the opposite direction of foldLeft so you have to reverse the list
    foldLeft(reverse(l), z)((z,l) => f(l,z))
  }

  def foldLeftWithRight[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    foldRight(l, (b:B) => b)((a,g) => b => g(f(b,a)))(z)
  }
  /*
  Exercise 3.15
  Hard: Write a function that concatenates a list of lists into a single list.
  Its runtime should be linear in the total length of all lists.
  Try to use functions we have already defined.
   */
  def flattenList[A](nestedList: List[List[A]]):List[A] = {
    foldRightWithLeft(nestedList, Nil:List[A])((x,z) => append(x,z))
    //solution from answer key
    //foldRight(l, Nil:List[A])(append)
  }

  /*
  Exercise 3.16
  Write a function that transforms a list of integers by adding 1 to each element.
  (Reminder: this should be a pure function that returns a new List!)
   */
  def incrementByOne(list: List[Int]): List[Int] = {
    foldRightWithLeft(list, Nil:List[Int])((x,z) => Cons(x + 1, z))
  }

  /*
  Exercise 3.17
  Write a function that turns each value in a List[Double] into a String.
  You can use the expression d.toString to convert some d: Double to a String.
   */
  def maptoString(list: List[Double]): List[String] = {
    foldRightWithLeft(list, Nil:List[String])((x,z) => Cons(x.toString, z))
  }

  /*
  Exercise 3.18
  Write a function map that generalizes modifying each element in a list while maintaining
  the structure of the list.
   */
  def map[A, B](l: List[A])(f: A => B): List[B] = {
    foldRightWithLeft(l, Nil:List[B])((x,z) => Cons(f(x), z))
  }

  /*
  3.19
  Write a function filter that removes elements from a list unless they satisfy a given predicate.
  Use it to remove all odd numbers from a List[Int].
   */
  def filter[A](list: List[A])(f: A => Boolean): List[A] = {
    foldRightWithLeft(list, Nil:List[A])((x,z) => if(f(x)) Cons(x,z) else z)
  }

  /*
  3.20
  Write a function flatMap that works like map except that the function given will return a list
  instead of a single result, and that list should be inserted into the final resulting list.

  For instance, flatMap(List(1,2,3))(i => List(i,i)) should result in
  List(1,1,2,2,3,3).
   */
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
    flattenList(map(as)(f))
  }

  /*
  Exercise 3.21
  Use flatMap to implement filter.
   */
  def filterViaFlatMap[A](list: List[A])(f: A => Boolean): List[A] = {
    flatMap(list)(a => if(f(a)) List(a) else Nil)
  }

  /*
  Exercise 3.22
  Write a function that accepts two lists and constructs a new list by adding corresponding elements.
  For example, List(1,2,3) and List(4,5,6) become List(5,7,9)
   */
  def addLists(listA: List[Int], listB: List[Int]) : List[Int] = {
    //didn't know could match on lists like this, had to look at solution
    (listA, listB) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(a, as), Cons(b, bs)) => Cons(a+b, addLists(as, bs))
    }
  }
  /*
  Exercise 3.23
  Generalize the function you just wrote so that it’s not specific to integers or addition.
  Name your generalized function zipWith.
   */
  def zipWith[A,B,C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = {
    (a,b) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(a, as), Cons(b, bs)) => Cons(f(a,b), zipWith(as, bs)(f))
    }
  }

  /*
  Exercise 3.24
  Hard: As an example, implement hasSubsequence for checking whether a List contains another List as a subsequence.
  For instance, List(1,2,3,4) would have List(1,2), List(2,3), and List(4) as subsequences, among others.
  You may have some difficulty finding a concise purely functional implementation that is also efficient.
  That’s okay. Implement the function however comes most naturally.
  We’ll return to this implementation in chapter 5 and hopefully improve on it.
  Note: Any two values x and y can be compared for equality in Scala using the expression x == y.
   */
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    println(s"matching on $sup and $sub")
    (sup, sub) match {
      case (_, Nil) => true
      case (Nil, Cons(_,_)) => false
      case (Cons(x,xs), Cons(y,ys)) => if (x != y) hasSubsequence(xs, sub) else hasSubsequence(xs, ys)
    }
  }
}
