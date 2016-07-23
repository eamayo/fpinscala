//package fpinscala.laziness

//  // EA Note: how to compile my code and make it available to the scala REPL
//  scalac -d classes MyStream.scala
//  scala -cp classes

import MyStream._
trait MyStream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the MyStream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h,t) => h() :: t().toList
  }
    
  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  
  def take(n: Int): MyStream[A] = this match {
    case Cons(h,t) if n > 0 => cons(h(), t().take(n-1))
    case _ => empty
  }

  @annotation.tailrec
  final def drop(n: Int): MyStream[A] = this match {
    case Cons(_,t) if n > 0 => t().drop(n-1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): MyStream[A] = this match {
    case Cons(h,t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }
  
  def takeWhileViaFoldRight(p: A => Boolean): MyStream[A] = 
    foldRight(empty[A])((a,b) => if (p(a)) cons(a, b) else empty)

  def forAll(p: A => Boolean): Boolean = 
    foldRight(true)((a,b) => p(a) && b)

  def headOption: Option[A] = 
    foldRight(None: Option[A])((a,b) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B): MyStream[B] = 
    foldRight(empty[B])((a, b) => cons(f(a), b))
    
  def filter(p: A => Boolean): MyStream[A] = 
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else b)
    
  def append[B >: A](xs: => MyStream[B]): MyStream[B] = 
    foldRight(xs)((a, b) => cons(a, b))
    
  def flatMap[B](f: A => MyStream[B]): MyStream[B] = 
    foldRight(empty[B])((a, b) => f(a) append b)
    
  def mapViaUnfold[B](f: A => B): MyStream[B] = 
    unfold(this){ case Cons(h, t) => Some((f(h()), t())) ; case _ => None}
  
  def takeViaUnfold(n: Int): MyStream[A] = 
    unfold((n, this)){
      case (n, Cons(h, t)) if n > 0 =>  Some((h(), (n-1, t())))
      case _ => None
    }
  
  def takeWhileViaUnfold(p: A => Boolean): MyStream[A] = 
    unfold(this){
      case Cons(h, t) if p(h()) =>  Some((h(), t())) 
      case _ => None
    }
  
  def zipWith[B, C](s: MyStream[B])(f: (A, B) => C): MyStream[C] = 
    unfold((this, s)){
      case (Cons(h1, t1), Cons(h2, t2)) =>  Some((f(h1(),h2()), (t1(), t2())))
      case _ => None
    }
  
  
  // Example from the companion booklet: A special case of `zipWith` - Python-style zip()
  def zip[B](s2: MyStream[B]): MyStream[(A,B)] =
    zipWith(s2)((_,_))
  
  // My solution to zipAll
  def zipAll[B](s: MyStream[B]): MyStream[(Option[A], Option[B])]  = 
    unfold((this, s)){
      case (Cons(h1, t1), Cons(h2, t2)) =>  Some((Some(h1()), Some(h2())), (t1(), t2()))
      case (Empty, Cons(h2, t2)) =>  Some((None, Some(h2())), (Empty, t2()))
      case (Cons(h1, t1), Empty) =>  Some((Some(h1()), None), (t1(), Empty))
      case _ => None
    }
  
  
  // Another Solution to zipAll from the companion booklet that uses zipWithAll 
  def zipWithAll[B, C](s: MyStream[B])(f: (Option[A], Option[B]) => C): MyStream[C] =
    unfold((this, s)){
      case (Cons(h1, t1), Cons(h2, t2)) =>  Some(f(Some(h1()), Some(h2())), (t1(), t2()))
      case (Empty, Cons(h2, t2)) =>  Some(f(None, Some(h2())), (Empty, t2()))
      case (Cons(h1, t1), Empty) =>  Some(f(Some(h1()), None), (t1(), Empty))
      case _ => None
    }
  
  def zipAll2[B](s: MyStream[B]): MyStream[(Option[A], Option[B])]  = 
    zipWithAll(s)((_,_))
  
  // First I need a function that will zip the 2 streams but unlike zip or zipAll will stop only when the 2nd
  // stream is exhausted. This will allow me to detect if the second stream is longer than the first one (consequently
  // s1 startsWith s2 would be false)
  def zipUntilAllSecondStream[B](s: MyStream[B]): MyStream[(Option[A], B)] = 
    unfold((this, s)){
      case (Cons(h1, t1), Cons(h2, t2)) =>  Some((Some(h1()), h2()), (t1(), t2()))
      case (Empty, Cons(h2, t2)) =>  Some((None, h2()), (Empty, t2()))
      case _ => None
    }
  
  // Now it should be easy to check if (s1 startsWith s2) since it's   
  //   True if all elements of the stream (s1 zipUntilAllSecondStream s2) satisfy (Some(a), a)
  //   False if some element of the stream (s1 zipUntilAllSecondStream s2) satisfies (None, b)
  def startsWith[A](s: MyStream[A]): Boolean = 
    zipUntilAllSecondStream(s) forAll {
      case (Some(a), b) => a == b
      case (None, _) => false
    }
  
  /* 
   * Solution from the companion booklet that uses the standard Stream methods
   *  `s startsWith s2` when corresponding elements of `s` and `s2` are all equal, until the point 
   *  that `s2` is exhausted. If `s` is exhausted first, or we find an element that doesn't match, 
   *  we terminate early. Using non-strictness, we can compose these three separate logical steps - the zipping, 
   *  the termination when the second stream is exhausted, and the termination if a non-matching element is found 
   *  or the first stream is exhausted.
  */
  def startsWith2[A](s: MyStream[A]): Boolean = 
    zipAll(s).takeWhile(!_._2.isEmpty) forAll {
      case (h,h2) => h == h2
    }

  def tails: MyStream[MyStream[A]] = 
    unfold(this) { 
      case Empty => None
      case s => Some((s, s drop 1)) 
    } append MyStream(empty)
  
//  // testing tails
//  scala> val test = MyStream(1,2,3).tails
//  test: MyStream[MyStream[Int]] = Cons(<function0>,<function0>)

//  scala> test.map(_.toList).toList
//  res7: List[List[Int]] = List(List(1, 2, 3), List(2, 3), List(3), List())
    
//  scala> val from3 = MyStream.from(3)
//  from3: MyStream[Int] = Cons(<function0>,<function0>)

//  scala> from3.tails.map(_.take(2).toList).take(10).toList
//	res5: List[List[Int]] = List(List(3, 4), List(4, 5), List(5, 6), List(6, 7), List(7, 8), List(8, 9), List(9, 10), List(10, 11), List(11, 12), List(12, 13))
//  
  def hasSubsequence[A](s: MyStream[A]): Boolean = 
    tails exists (_ startsWith s)
  
  def scanRight[B](z: B)(f: (A, B) => B): MyStream[B] = 
    foldRight((z, MyStream(z))){
      case (a, (b, s)) => {
        val nextResult = f(a,b)
        (nextResult, cons(nextResult, s))
      }
    }._2
    
// // testing my solution for scanRight
//    scala> MyStream(1,2,3).scanRight(0)(_+_).toList
//    res2: List[Int] = List(6, 5, 3, 0)
    
//  // Another test: using scanRight to generate the output of tails
//  scala> MyStream(1,2,3).scanRight(MyStream.empty[Int])((a,b) => MyStream.cons(a,b)).map(_.toList).toList
//  res9: List[List[Int]] = List(List(1, 2, 3), List(2, 3), List(3), List())
    
  // Now I can define tails as a special case of scanRight
  def tailsViaScanRight: MyStream[MyStream[A]] = 
    this.scanRight(empty[A])((a,b) => cons(a,b))
  
  // solution from the companion booklet that improves on mine slightly by using lazy-val
  // to make sure the pass-by-name foldRight argument is cached in our computation and only used once
  def scanRight2[B](z: B)(f: (A, => B) => B): MyStream[B] =
    foldRight((z, MyStream(z)))((a, p0) => {
      // p0 is passed by-name and used in by-name args in f and cons. So use lazy val to ensure only one evaluation...
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, cons(b2, p1._2))
    })._2
        
}

case object Empty extends MyStream[Nothing]
case class Cons[+A](h: () => A, t: () => MyStream[A]) extends MyStream[A]

object MyStream {
  def cons[A](hd: => A, tl: => MyStream[A]): MyStream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: MyStream[A] = Empty

  def apply[A](as: A*): MyStream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: MyStream[Int] = cons(1, ones)
  
  def constant[A](a: A): MyStream[A] = {
      lazy val constantMyStream: MyStream[A] = cons(a, constantMyStream)
      constantMyStream
    }
  
  def from(n: Int): MyStream[Int] = cons(n, from(n+1))
  
  def fibs(cur: Int, next: Int): MyStream[Int] = cons(cur, fibs(next, cur + next))
  
  val fibonacciNumbers = fibs(0,1)
  
  // fibonacciNumbers.take(10).toList

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): MyStream[A] = f(z) match {
    case None => empty[A]
    case Some((value, nextState)) => cons(value, unfold(nextState)(f))
  }
  
  val fibsViaUnfold = unfold((0,1)){ case (cur, next) => Some((cur, (next, cur + next)))}
  
  def constantViaUnfold[A](a: A) = unfold(a)(x => Some(x,x))
  
  def fromViaUnfold(n: Int) = unfold(n)(x => Some((x, x+1)))

}