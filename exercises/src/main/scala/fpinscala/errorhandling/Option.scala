package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(v) => Some(f(v))
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case None => default
    case Some(v) => v
  }

  def flatMap[B](f: A => Option[B]): Option[B] = 
    map(f) getOrElse None

  def orElse[B>:A](ob: => Option[B]): Option[B] = 
    map(Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(v) if (f(v)) => Some(v)
    case _ => None
  }
  
  // I could also implement the above by reusing the previously defined functions
  def filterNoPatternMatch(f: A => Boolean): Option[A] = 
    flatMap(a => if (f(a)) Some(a) else None)
  
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
    
  def variance(xs: Seq[Double]): Option[Double] = {
    val sample_mean: Option[Double] = mean(xs)
    sample_mean flatMap (x => mean(xs.map(a => math.pow(a - x,2))) )
  }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a,b) match {
    case (Some(x), Some(y)) => Some(f(x,y))
    case _ => None
  }
  
  // another version of map2 using the Option methods I just implemented
  def map2_2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = 
    a flatMap (x =>  b map (y => f(x,y) ) )
  
  def map3[A,B,C,D](a: Option[A], b: Option[B], c: Option[C])(f: (A, B, C) => D): Option[D] = 
    a flatMap (x =>  b flatMap (y => c map (z => f(x, y, z) ) ) )
  
  def map4[A,B,C,D,E](a: Option[A], b: Option[B], c: Option[C], d: Option[D])(f: (A, B, C, D) => E): Option[E] = 
    a flatMap (u =>  b flatMap (v => c flatMap (w => d map (x => f(u,v,w,x) ) ) ) )

  def sequence[A](a: List[Option[A]]): Option[List[A]] = sys.error("todo")

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = sys.error("todo")
}