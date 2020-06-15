package fpinscala.laziness

import Stream._

import scala.annotation.tailrec

trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  /*
  Exercise 5.2
  implement take and drop
   */
  def take(n: Int): Stream[A] = {
    n match {
      case a if a < 0 => sys.error("n must be greater than or equal to 0")
      case 0 => Empty
      case _ => {
        this match {
          case Empty => Empty
          case Cons(h, t) => cons(h(), t().take(n - 1))
        }
      }
    }
  }

  @tailrec
  final def drop(n: Int): Stream[A] = {
    n match {
      case a if a < 0 => sys.error("n must be greater than or equal to 0")
      case 0 => this
      case _ => {
        this match {
          case Empty => Empty
          case Cons(_, t) => t().drop(n - 1)
        }
      }
    }
  }

  //Exercise 5.3 and 5.5
  def takeWhile(p: A => Boolean): Stream[A] = {
    this.foldRight(empty: Stream[A])((a, z) => if (p(a)) cons(a, z) else z)
  }

  /*Exercise 5.4
  Implement forAll, which checks that all elements in the Stream match a given predi- cate. Your implementation should
  terminate the traversal as soon as it encounters a nonmatching value.
   */
  def forAll(p: A => Boolean): Boolean = {
    this.foldRight(true)((a, z) => p(a) && z)
  }

  /*
  Exercise 5.6
  Hard: Implement headOption using foldRight.
   */
  def headOption: Option[A] = {
    //without foldRight => Option(take(1))
    foldRight(None: Option[A])((a, z) => Some(a))
  }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def startsWith[B](s: Stream[B]): Boolean = ???

  /*
  Exercise 5.1
  Write a function to convert a Stream to a List, which will force its evaluation
  and let you look at it in the REPL. You can convert to the regular List type in the
  standard library. You can place this and other functions that operate on a Stream
  inside the Stream trait.
   */
  def toList: List[A] = {
    this match {
      case Cons(h, t) => t().toList.::(h())
      case Empty => Nil
    }
    /*toList solution from the answer key used a tail recursive inner function to avoid possible stack overflow
    toListFast solution uses a mutable buffer to create the list using a tail recursive function
     */

  }

  /*
  Exercise 5.7
  Implement map, filter, append, and flatMap using foldRight. The append method should be non-strict in its argument.
   */

  def map[B](f: A => B): Stream[B] = {
    foldRight(empty[B])((a, z) => cons(f(a), z))
  }

  def filter(f: A => Boolean): Stream[A] = {
    foldRight(empty[A])((a, z) => if (f(a)) cons(a, z) else z)
  }

  def append[B >: A](s: => Stream[B]): Stream[B] = {
    foldRight(s)((a, z) => cons(a, z))
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(empty[B])((a, z) => f(a).append(z))
  }

}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  /*
  Exercise 5.8
  Generalize ones slightly to the function constant, which returns an infinite Stream of a given value.
   */
  def constant[A](a: A): Stream[A] = {
    /*
    my initial solution
    val infinite: Stream[A] = cons(a, constant(a))
    infinite
    */
    //answer key's solution
    lazy val infinite: Stream[A] = Cons(() => a, () => infinite)
    infinite
  }

  /*
  Exercise 5.9
  Write a function that generates an infinite stream of integers, starting from n, then n + 1, n + 2, and so on
   */
  def from(n: Int): Stream[Int] = {
    cons(n, from(n + 1))
  }

  /*
  Exercise 5.10
  Write a function fibs that generates the infinite stream of Fibonacci numbers: 0, 1, 1, 2, 3, 5, 8, and so on.
   */
  def fibs(): Stream[Int] = {
    def innerFib(prev: Int = 0, current: Int = 1): Stream[Int] = {
      cons(prev, innerFib(current, prev + current))
    }
    innerFib()
  }

  /*
  Exercise 5.11
  Write a more general stream-building function called unfold.
  It takes an initial state, and a function for producing both the next state
  and the next value in the generated stream.
   */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    //followed types to implementation, section in book mentioned Option is used to terminate
    f(z) match {
      case None => empty
      case Some((nextValue, nextState)) => cons(nextValue, unfold(nextState)(f))
    }
    /*
    solution from answer key's variable names help to understand it better
    f(z) match {
    case Some((h,s)) => cons(h, unfold(s)(f))
    case None => empty
     */
  }
}