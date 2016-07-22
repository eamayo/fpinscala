//package fpinscala.laziness

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
    unfold(headOption)(state => state.map(x => (f(x), drop(1).headOption)))
  
//  TODO: need to debug (using fromN example)
  def takeViaUnfold(n: Int): MyStream[A] = 
    unfold((n, headOption)){case (n, h) => if (n > 0) h.map(x => (x, (n-1, drop(1).headOption))) else None }
  
//  TODO: need to debug (using fromN example)
  def takeWhileViaUnfold(p: A => Boolean): MyStream[A] = 
    unfold(headOption)(state => state.filter(p).map(x => (x, drop(1).headOption)))
  
  def zipWith[B, C](s: MyStream[B])(f: (A, B) => C): MyStream[C] = ???
  
  

  def startsWith[B](s: MyStream[B]): Boolean = sys.error("todo")
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