package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }
  
  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(x) => x 
    case Branch(left, right) => maximum(left)  max maximum(right)
  }
  
  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 0
    case Branch(left, right) => 1 + (depth(left) max depth(right))
  }
  
  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(v) => Leaf(f(v))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }
  
  def fold[A, B](tree: Tree[A])(g: A => B)(f: (B, B) => B): B = tree match {
    case Leaf(v) => g(v)
    case Branch(left, right) => f(fold(left)(g)(f), fold(right)(g)(f))
  }
  
  def sizeViaFold[A](tree: Tree[A]): Int = 
      fold(tree)(leafVal => 1)((lSize, rSize) => 1 + lSize + rSize)
      
  def maximumViaFold(tree: Tree[Int]): Int = 
      fold(tree)(identity[Int])((lMax: Int, rMax: Int) => lMax max rMax)
  
  def depthViaFold[A](tree: Tree[A]): Int =
      fold(tree)(_ => 0)((lDepth: Int, rDepth: Int) => 1 + (lDepth max rDepth))
    
      
  /*
      Aspects of Scala I learnt while doing this exercise:
      
      Note the type annotation required on the expression `Leaf(f(a))`. Without this annotation, we get an error like this: 
      type mismatch;
        found   : fpinscala.datastructures.Branch[B]
        required: fpinscala.datastructures.Leaf[B]
           fold(t)(a => Leaf(f(a)))(Branch(_,_))
                                          ^  
      This error is an unfortunate consequence of Scala using subtyping to encode algebraic data types. Without the annotation, 
      the result type of the fold gets inferred as `Leaf[B]` and it is then expected that the second argument to `fold` will 
      return `Leaf[B]`, which it doesn't (it returns `Branch[B]`). Really, we'd prefer Scala to infer `Tree[B]` as the result type 
      in both cases. 
      
      When working with algebraic data types in Scala, it's somewhat common to define helper functions that simply call the 
      corresponding data constructors but give the less specific result type:  
        
        def leaf[A](a: A): Tree[A] = Leaf(a)
        def branch[A](l: Tree[A], r: Tree[A]): Tree[A] = Branch(l, r)
  */
  def mapViaFold[A, B](tree: Tree[A])(h: A => B): Tree[B] =
      fold(tree)(leafVal => Leaf(h(leafVal)): Tree[B])((l: Tree[B], r: Tree[B]) => Branch(l, r))

}