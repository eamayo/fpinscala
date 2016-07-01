package fpinscala.datastructures
 

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = 
    l match {
      case Nil => sys.error("tail of empty list")
      case Cons(h, t) => t
    }

  def setHead[A](l: List[A], h: A): List[A] = 
    l match {
      case Nil => sys.error("cannot set head of empty list")
      case Cons(x, xs) => Cons(h, xs)
    }

  def drop[A](l: List[A], n: Int): List[A] = {
    @annotation.tailrec
    def go(xs: List[A], countReverse: Int): List[A]  = 
      (xs, countReverse) match{
        case (_, j) if j < 0 => sys.error("invalid argument. n must be nonnegataive")
        case (Nil, j) if j > 0 => sys.error("cannot drop more elements than list contains")
        case (ys, 0)  => ys
        case (Cons(h,t), j) => go(t, j-1)
      }
    
    go(l, n)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    @annotation.tailrec
    def go(as: List[A]): List[A]  = 
      as match{
        case Cons(x, xs) if f(x) => go(xs)
        case _ => as
      }
    
    go(l)
  }

  def init[A](l: List[A]): List[A] = 
    l match {
      case Nil => sys.error("init of empty list")
      case Cons(_, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_,n) => n + 1)

  // EA review comment: operators on list of elements (A,B,C,D) that turn it into
  // A op B op C op D are: 
  //  (1) left-associative if 
  //   A op B op C op D  means ( ( (initVal op A ) op B ) op C ) op D 
  //  for left-associative operators use foldLeft
  //  (2) right-associative if 
  //   A op B op C op D  means A op ( B op ( C op ( D op initVal ) ) ) 
  //  for right-associative operators use foldRight
  //
  // Obviously if the operator is associative then foldLeft and foldRight will
  // have the same output value but, depending on the underlying implementations
  // may differ in terms of efficiency since foldLeft can be implemented naturally 
  // as a tail-recursive function
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    @annotation.tailrec
    def go(rem: List[A], acc: B): B = rem match {
      case Nil => acc
      case Cons(x, xs) => go(xs, f(acc, x))
    }  
    go(l, z)
  }
  
  def sum3(ns: List[Int]) = foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Double]) = foldLeft(ns, 1.0)(_ * _)
  
  def length2[A](l: List[A]): Int = foldLeft(l, 0)((n,_) => n + 1)
  
  def reverse[A](l: List[A]): List[A] = 
    foldLeft(l, Nil: List[A])((acc, x) => Cons(x, acc))
  
  def foldLeftNotStackSafe[A, B](l: List[A], z: B)(f: (B, A) => B): B = 
    foldRight(reverse(l), z)((x, acc) => f(acc, x))
  
  def foldRightStackSafe[A, B](l: List[A], z: B)(f: (A, B) => B): B = 
    foldLeft(reverse(l), z)((acc, x) => f(x, acc))
  
  def append2[A](a1: List[A], a2: List[A]): List[A] = 
    foldRightStackSafe(a1, a2)(Cons(_,_))
  
  def concatenate[A](ll: List[List[A]]): List[A] = 
    foldRightStackSafe(ll, Nil: List[A])(append2)
  
  def add1(l: List[Int]): List[Int] = 
    foldRightStackSafe(l, Nil: List[Int])((x,xs) => Cons(x+1, xs))
  
  def eachToString(l: List[Double]): List[String] = 
    foldRightStackSafe(l, Nil: List[String])((x,xs) => Cons(x.toString(), xs))

  def map[A,B](l: List[A])(f: A => B): List[B] = 
    foldRightStackSafe(l, Nil: List[B])((x,xs) => Cons(f(x), xs))
  
  def filter[A](l: List[A])(f: A => Boolean): List[A] = 
    foldRightStackSafe(l, Nil: List[A])((x, xs) => if (f(x)) Cons(x, xs) else xs)
    
//    Testing filter
//  scala> val xs: List[Int] = List(1,2,3,4,5,6)
//  xs: List[Int] = Cons(1,Cons(2,Cons(3,Cons(4,Cons(5,Cons(6,Nil))))))
//  
//  scala> filter(xs)(x => (x % 2) == 0)
//  res4: List[Int] = Cons(2,Cons(4,Cons(6,Nil)))

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = concatenate(map(l)(f))
  
  def addCorresponding(a1: List[Int], a2: List[Int]): List[Int] = (a1, a2) match {
    case (Nil, ys) => Nil
    case (xs, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(x+y, addCorresponding(xs, ys))
  }
  
  def zipWith[A, B, C](a1: List[A], a2: List[B])(f: (A, B) => C): List[C] = (a1, a2) match {
    case (Nil, ys) => Nil
    case (xs, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x,y), zipWith(xs, ys)(f))
  }
  
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    @annotation.tailrec
    def hasPrefix(l: List[A], prefix: List[A], matchSoFar: Boolean): Boolean = (l, prefix, matchSoFar) match {
      case (_, _, false) => false   // match already failed so don't go any further
      case (Nil, Nil, s) => s       // we've exhausted both lists so return matchSoFar
      case (Nil, ys, _) => false    // sup is shorter than sub
      case (xs, Nil, s) => s        // if we've exhausted sub return return matchSoFar; This also allows us to treat Nil as a subsequence of every List
      case (Cons(x, xs), Cons(y, ys), s) => hasPrefix(xs, ys, (x == y) && s)  // sub is a prefix of sup <=> (sup.head == sup.head) and (sub.tail is a prefix of sup.tail) 
    }      
    
    // We loop over the elements of sup until we find a sublist that has sub as a prefix
    @annotation.tailrec
    def go(cur: List[A])(f: (List[A], List[A], Boolean) => Boolean): Boolean = cur match {
      case Nil => false
      case _ if f(cur, sub, true) => true
      case Cons(h, t)  => go(t)(f)
    }
    
    go(sup)(hasPrefix)
  }
  
  
  // We can generalize hasSubsequence() as follows
  // Let sub = List(sub_1, sub_2, ....,sub_m) and sup = List(sup_1, sup_2,..., sup_n)
  // We'll say that sup has subsequence satisfying condition f if there exists a k in the range [1, n] such that 
  // k + m -1 <= n and f(sup_(k + i - 1), sub_i) is true for i = 1, ..., m 
  def hasSubsequenceSatisfying[A, B](sup: List[A], sub: List[B])(f: (A, B) => Boolean): Boolean = {
    @annotation.tailrec
    def hasPrefix(l: List[A], prefix: List[B], matchSoFar: Boolean): Boolean = (l, prefix, matchSoFar) match {
      case (_, _, false) => false   // match already failed so don't go any further
      case (Nil, Nil, s) => s       // we've exhausted both lists so return matchSoFar
      case (Nil, ys, _) => false    // sup is shorter than sub
      case (xs, Nil, s) => s        // if we've exhausted sub return return matchSoFar; This also allows us to treat Nil as a subsequence of every List
      case (Cons(x, xs), Cons(y, ys), s) => hasPrefix(xs, ys, f(x, y) && s)  // sub is a prefix of sup <=> f(sup.head , up.head) and (sub.tail is a prefix of sup.tail) 
    }      
    
    // We loop over the elements of sup until we find a sublist that has sub as a prefix
    @annotation.tailrec
    def go(cur: List[A])(g: (List[A], List[B], Boolean) => Boolean): Boolean = cur match {
      case Nil => false
      case _ if g(cur, sub, true) => true
      case Cons(h, t)  => go(t)(g)
    }
    
    go(sup)(hasPrefix)
  }
    
  
}
