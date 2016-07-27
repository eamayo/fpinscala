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
  
  def doubleViaMap: Rand[Double] =
    map(nonNegativeInt)(n => n.toDouble/(Int.MaxValue.toDouble +1 ))

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = 
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a,b), rng3)
    }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = 
    rng => fs.foldRight((Nil:List[A], rng)){
      case (action, (l, r)) => val (x, r2) = action(r); (x :: l, r2) 
    }
    
  // ** An alternative solution from the companion booklet to sequence that uses map2
  
  // In `sequence`, the base case of the fold is a `unit` action that returns
  // the empty list. At each step in the fold, we accumulate in `acc`
  // and `f` is the current element in the list.
  // `map2(f, acc)(_ :: _)` results in a value of type `Rand[List[A]]`
  // We map over that to prepend (cons) the element onto the accumulated list.
  //
  // We are using `foldRight`. If we used `foldLeft` then the values in the
  // resulting list would appear in reverse order. It would be arguably better
  // to use `foldLeft` followed by `reverse`. What do you think?
  //  
  // It's interesting that we never actually need to talk about the `RNG` value
  // in `sequence`. This is a strong hint that we could make this function
  // polymorphic in that type.
  def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))
 
    
  def intsViaSequence[A](count: Int): Rand[List[Int]] = 
    sequence(List.fill(count)(int))
    
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    val h = (s: (A, RNG)) => g(s._1)(s._2)
    h compose f
  }
  
  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt)( x => {
      val mod = x % n
      if (x + (n - 1) - mod >= 0) 
        rng => (mod, rng)  // I could have re-used unit(mod) here 
      else 
        nonNegativeLessThan(n)
    })
    
  def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] = 
    flatMap(s)(a => unit(f(a)))
  
  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =  
    flatMap(ra)(a => flatMap(rb)(b => unit(f(a,b))))  // could have simplified this to flatMap(ra)(a => map(rb)(b => f(a,b)))
}

import State._

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = 
    State(s => {
      val (a, s2) = run(s)
      (f(a), s2)
      }
    )
    
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    State(s => {
      val (a, s2) = run(s)
      val (b, s3) = sb.run(s2)
      (f(a,b), s3)
      }
    )
    
  def flatMap[B](f: A => State[S, B]): State[S, B] = {
    val h = (s: (A, S)) => f(s._1).run(s._2)
    State(h compose run)
  }
  
  def mapViaFlatMap[B](f: A => B): State[S, B] = 
    flatMap(a => unit(f(a)))
    
  def map2ViaFlatMap[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a,b)))
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  
   def unit[S, A](a: A): State[S,A] =
    State(s => (a, s))
  
  def sequence[S, A](fs: List[State[S,A]]): State[S, List[A]] =
    fs.foldRight(unit[S,List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))
    
  // ** More efficient Alternative solutions from the companion booklet to sequence
  
  // This implementation uses a loop internally and is the same recursion
  // pattern as a left fold. It is quite common with left folds to build
  // up a list in reverse order, then reverse it at the end.
  // (We could also use a collection.mutable.ListBuffer internally.)
  def sequenceViaLoop[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
    def go(s: S, actions: List[State[S,A]], acc: List[A]): (List[A],S) =
      actions match {
        case Nil => (acc.reverse,s)
        case h :: t => h.run(s) match { case (a,s2) => go(s2, t, a :: acc) }
      }
    State((s: S) => go(s,sas,List()))
  }

  // We can also write the loop using a left fold. This is tail recursive like the
  // previous solution, but it reverses the list _before_ folding it instead of after.
  // You might think that this is slower than the `foldRight` solution since it
  // walks over the list twice, but it's actually faster! The `foldRight` solution
  // technically has to also walk the list twice, since it has to unravel the call
  // stack, not being tail recursive. And the call stack will be as tall as the list
  // is long.
  def sequenceViaFoldLeft[S,A](l: List[State[S, A]]): State[S, List[A]] =
    l.reverse.foldLeft(unit[S, List[A]](List()))((acc, f) => f.map2(acc)( _ :: _ ))

  
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
