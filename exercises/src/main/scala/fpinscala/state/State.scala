package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {

  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  /*
  Exercise 6.1
  Write a function that uses RNG.nextInt to generate a random integer between 0 and Int.maxValue (inclusive).
  Make sure to handle the corner case when nextInt returns Int.MinValue, which doesn’t have a non-negative counterpart.
   */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    rng.nextInt match {
      case (Int.MinValue, nextRng) => (Int.MaxValue, nextRng)
      case (random, nextRng) => (random.abs, nextRng)
    }
    /* solution from answer key as an alternative
    def nonNegativeInt(rng: RNG): (Int, RNG) = {
      val (i, r) = rng.nextInt
      (if (i < 0) -(i + 1) else i, r)
    }
     */
  }

  /*
  Exercise 6.2
  Write a function to generate a Double between 0 and 1, not including 1. Note: You can use Int.MaxValue to obtain the
  maximum positive integer value, and you can use x.toDouble to convert an x: Int to a Double.
   */
  def double(rng: RNG): (Double, RNG) = {
    /*
    val (random, nextRNg) = nonNegativeInt(rng)
    def enforceUpperBound(r: Double): Double ={
      if (r > 1) enforceUpperBound(r / 10) else r
    }
    val randomDouble = random.toDouble
    val boundedRandomDouble = enforceUpperBound(randomDouble)
    (boundedRandomDouble, nextRNg)
    */
    //solution from answer key has a much simpler and efficient way of enforcing the upper bound of 1 (non-inclusive)
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  /*
  Exercise 6.3
  Write functions to generate an (Int, Double) pair, a (Double, Int) pair, and a (Double, Double, Double) 3-tuple.
  You should be able to reuse the functions you’ve already written.
   */
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (rndInt, nextRng) = nonNegativeInt(rng)
    val (rndDouble, lastRng) = double(nextRng)
    ((rndInt, rndDouble), lastRng)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (rndDouble, nextRng) = double(rng)
    val (rndInt, lastRng) = nonNegativeInt(nextRng)
    ((rndDouble, rndInt), lastRng)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (firstRnd, firstRng) = double(rng)
    val (secondRnd, secondRng) = double(firstRng)
    val (thirdRnd, thirdRng) = double(secondRng)
    ((firstRnd, secondRnd, thirdRnd), thirdRng)
  }

  /*
  Exercise 6.4
  Write a function to generate a list of random integers.
   */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def innerInts(count: Int)(innerRng: RNG)(list: List[Int]): (List[Int], RNG) = {
      if (count == 0) {
        (list, innerRng)
      } else {
        val (i, r) = innerRng.nextInt
        innerInts(count - 1)(r)(i :: list)
      }
    }

    innerInts(count)(rng)(List.empty[Int])
  }

  /*
  Exercise 6.5
  Use map to reimplement double in a more elegant way. See exercise 6.2.
   */
  def doubleViaMap: Rand[Double] = {
    map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))
    //can also use _ instead of i => i
  }

  /*
  Exercise 6.6
  Write the implementation of map2 based on the following signature. This function takes two actions, ra and rb, and
  a function f for combining their results, and returns a new action that combines them
   */
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng1) = ra(rng)
      val (b, rng2) = rb(rng1)
      (f(a, b), rng2)
    }
  }

  /*
  Exercise 6.7
  Hard: If you can combine two RNG transitions, you should be able to combine a whole list of them. Implement sequence
  for combining a List of transitions into a single transition. Use it to reimplement the ints function you wrote
  before. For the latter, you can use the standard library function List.fill(n)(x) to make a list with x repeated n
  times.
   */
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    //used foldLeft initially, but realized it would apply each Rand in the wrong order
    rng =>
      fs.foldRight((List.empty[A], rng)) { (rand, acc) =>
        val (a, nextRng) = rand(acc._2)
        (a :: acc._1, nextRng)
      }
    //see 07.answer.scala for more details
    //fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))
  }

  def intsViaSequence(count: Int): Rand[List[Int]] = {
    sequence(List.fill(count)(int))
  }

  /*
  Exercise 6.8
  Implement flatMap, and then use it to implement nonNegativeLessThan.
   */
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng =>
      val (a, nextRNG) = f(rng)
      g(a)(nextRNG)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) {
      i =>
        val mod = i % n
        if (i + (n - 1) - mod >= 0)
          unit(mod)
        else
          nonNegativeLessThan(n)
    }
  }

  /*
  Exercise 6.9
  Reimplement map and map2 in terms of flatMap. The fact that this is possible is what we’re referring to when we say
  that flatMap is more powerful than map and map2.
   */
  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    flatMap(s) {
      a =>
        val mapped = f(a)
        unit(mapped)
    }
  }

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    //can use map here instead and curry it to f like in answer from solution key
    /*flatMap(ra) {
      a =>
        flatMap(rb) {
          b => unit(f(a, b))
        }
    }*/
    flatMap(ra)(a => map(rb)(b => f(a, b)))
  }
}

case class State[S, +A](run: S => (A, S)) {
  /*
  Exercise 6.10
  Generalize the functions unit, map, map2, flatMap, and sequence.
  Add them as methods on the State case class where possible. Otherwise you should put them in a State companion object.
   */
  //solutions from answer key implement map and map2 using flatmap
  def map[B](f: A => B): State[S, B] =
    State {
      state =>
        val (a, s) = run(state)
        (f(a), s)
    }

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    State {
      state =>
        val (a, s1) = run(state)
        val (b, s2) = sb.run(s1)
        (f(a, b), s2)
    }

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State {
      state =>
        val (a, s1) = run(state)
        val fState = f(a)
        /*
        can just return fState.run(s1)
        val (b, s2) = fState.run(s1)
        (b, s2)*/
        fState.run(s1)
    }

}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))
    /*
    Didn't move unit to object initially
      State {
      state =>
        def init: State[S, List[A]] = State[S, List[A]](s => (List[A](), s))

        val generate = fs.foldRight(init) { (f, acc) =>
          acc.flatMap { listA =>
            State { s =>
              val (a, next) = f.run(s)
              (listA :+ a, next) }
          }
        }
        generate.run(state)
    }*/

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
