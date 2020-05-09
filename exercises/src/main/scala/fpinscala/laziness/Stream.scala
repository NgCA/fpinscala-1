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

  //exercise 5.3
  def takeWhile(p: A => Boolean): Stream[A] = {
    this.foldRight(empty: Stream[A])((a,z) => if (p(a)) cons(a, z) else z)
  }

  def forAll(p: A => Boolean): Boolean = ???

  def headOption: Option[A] = ???

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

  def from(n: Int): Stream[Int] = ???

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = ???
}