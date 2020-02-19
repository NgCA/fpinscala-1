package fpinscala.errorhandling


import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E,+A] {
  /*
  Exercise 4.6
  Implement versions of map, flatMap, orElse, and map2 on Either that operate on the
  Right value
   */
 def map[B](f: A => B): Either[E, B] = {
   this match {
     case Left(e) => Left(e)
     case Right(b) => Right(f(b))
   }
 }

 def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
   this match {
     case Left(e) => Left(e)
     case Right(b) => f(b)
   }
 }

 def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = {
   this match {
     case Left(_) => b
     case Right(x) => Right(x)
   }
 }

 def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
   flatMap(x => b.map(y => f(x,y)))
   //solution used for-comprehension to replace explicit calls to flatMap and map
   //for { a <- this; b1 <- b } yield f(a,b1)
 }
}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
  /*
  Exercise 4.7 implement traverse and sequence
   */
  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    es.foldLeft[Either[E, List[B]]](Right(Nil))((z, a) => z.map2(f(a))(_.::(_)))
  }

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] = {
    traverse(es)(a => a)
  }

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

}
/*
  Exercise 4.8
  def mkPerson(name: String, age: Int): Either[String, Person] = mkName(name).map2(mkAge(age))(Person(_, _))

  In this implementation, map2 is only able to report one error, even if both the name and
  the age are invalid.

  Solution:
  1. What would you need to change in order to report both errors? Would
  you change map2 or the signature of mkPerson?

  Could possibly make map2 return Either[Seq[EE],C] instead to return a Sequence of errors, however
  all the failures would need to return the same type.

  2. Or could you create a new data type that
  captures this requirement better than Either does, with some additional structure?

  As far as a new data type goes, it might be possible to create a new data constructor (case class) where
  it's apply takes in a Collection of E

  3. How would orElse, traverse, and sequence behave differently for that data type?
  They would need a way to accumulate the errors into the collection.  So they wouldn't short circuit upon
  the first failure
 */