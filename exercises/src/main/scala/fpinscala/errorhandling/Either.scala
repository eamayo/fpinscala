package fpinscala.errorhandling


import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E,+A] {
 def map[B](f: A => B): Either[E, B] = this match {
   case Left(x) => Left(x)
   case Right(x) => Right(f(x))
 }

 def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
   case Left(x) => Left(x)
   case Right(x) => f(x)
 }

 def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
   case Left(x) => b
   case Right(x) => Right(x)
 }

 def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = (this, b) match {
   case (Right(x), Right(y)) => Right(f(x,y))
   case (Right(x), Left(y)) => Left(y)
   case (Left(x), _) => Left(x)
 }
 
 // another version of map2 using the methods I just implemented
 def map2_2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = 
   flatMap(x => b map (y => f(x, y)))
   
 // In chapter 4 page 59 the aside on for-comprehensions explains how Scala provides the for
 // comprehension as syntactic sugar for the nested sequence of flatMap calls finally ended by a map call 
 // as in my map2_2() function above or the map2, map3, map4 etc functions I wrote in the Option exercises.
 // Below is another version of map2 that uses the for-comprehension syntactic sugar
 def map2_3[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = 
   for {
     x <- this
     y <- b
   } yield f(x,y)
   
 def map3[EE >: E, B, C, D](b: Either[EE, B])(c: Either[EE, C])(f: (A, B, C) => D): Either[EE, D] = 
   for {
     x <- this
     y <- b
     z <- c
   } yield f(x,y,z)
 
}

case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
  // my own solution uses local mutation but still preserves RT
  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =  
    es.foldRight[Either[E, List[B]]](Right(Nil))((a, b) => f(a).map2_2(b)(_ :: _))

  // my first solution uses local mutation but still preserves RT
  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] = {
    import collection.mutable.ListBuffer
    val buf = new ListBuffer[A]
    @annotation.tailrec
    def go(cur: List[Either[E,A]]): Either[E, List[A]] = cur match {
      case Nil => Right(Nil)
      case Right(v) :: Nil => buf += v; Right(buf.toList)
      case Right(v) :: t => buf += v; go(t)
      case Left(v) :: _  => Left(v)
    }
    go(es)
      
  }
  
  // my second solution implements sequence in terms of traverse
  def sequence2[E,A](es: List[Either[E,A]]): Either[E,List[A]] =
    traverse(es)(identity)
  

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