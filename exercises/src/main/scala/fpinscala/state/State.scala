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

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, rng2) = rng.nextInt
    // Since there are more negative Ints than non-negative Ints,
    // in order not to break the uniformity of the generator we map
    // all negative Ints to their absolute value except for Int.MinValue
    // which gets mapped to zero. So the probabilities of getting each of 
    // the non-negative Ints will be equal 
    val m = if (n == Int.MinValue) 0 else math.abs(n)
    (m, rng2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (n, rng2) = nonNegativeInt(rng)
    (n.toDouble/(Int.MaxValue.toDouble +1 ), rng2)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (n, rng2) = nonNegativeInt(rng)
    val (r, rng3) = double(rng2)
    ((n,r), rng3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = intDouble(rng) match {
    case ((n,r), s) => ((r,n), s)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (r1, rng2) = double(rng)
    val (r2, rng3) = double(rng2)
    val (r3, rng4) = double(rng3)
    ((r1,r2,r3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @annotation.tailrec
    def go(n: Int)(state: (List[Int], RNG)): (List[Int], RNG) = (n, state) match {
      case (m, s) if m > 0 => {
        val (a, rng2) = s._2.nextInt
        go(n-1)((a :: s._1, rng2))
      }
      case _ => state 
    }
    
    go(count)((Nil, rng))
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = ???

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = ???

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    sys.error("todo")
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    sys.error("todo")
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    sys.error("todo")
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
