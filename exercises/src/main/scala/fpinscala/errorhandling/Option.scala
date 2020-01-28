package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  /*
  Exercise 4.1 implement map, getOrElse, flatMap, orElse, and filter
   */
  //Apply f if Option is not None
  def map[B](f: A => B): Option[B] = {
    this match {
      case None => None
      case Some(v) => Some(f(v))
    }
  }

  //return value of Option if Some else default
  def getOrElse[B>:A](default: => B): B = {
    this match {
      case None => default
      case Some(v) => v
    }
  }

  /*
  Tried to implement without using pattern matching, but ultimate went to the answer key.
  Took a while to understand solutions, but I've left comments to help.
   */

  //Applies f just like map, but will flatten result so you don't get Some(Some(_))
  def flatMap[B](f: A => Option[B]): Option[B] = {
    //taken from answer key
    map(f) getOrElse None
    //read as map(f).getOrElse(None)
  }

  //returns the first Option if it’s defined; otherwise, it returns the second Option.
  def orElse[B>:A](ob: => Option[B]): Option[B] = {
    //taken from answer key
    this map (Some(_)) getOrElse ob
    //passes in Some(_) as function to map, which will return Some(Some(_)) or None
    //this.map(Some(_)).getOrElse(ob)
  }

  //Convert Some to None if the value doesn’t satisfy f.
  def filter(f: A => Boolean): Option[A] = {
    //taken from answer key
    flatMap(a => if (f(a)) Some(a) else None)
    //applies predicate and returns Some or None depending on condition, flatMap makes sure we don't get nested Options
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  def variance(xs: Seq[Double]): Option[Double] = ???

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = ???

  def sequence[A](a: List[Option[A]]): Option[List[A]] = ???

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = ???
}