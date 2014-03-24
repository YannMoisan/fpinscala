package fpinscala.datastructures

sealed trait List[+A] // `List` data type
case object Nil extends List[Nothing] // data constructor for `List`
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object Test extends App {
  println(List.x)
  println(List.tail(List(1,2,3,4,5)))
  println(List.drop(List(1,2,3,4,5),2))
  println(List.dropWhile(List(1,2,3,4,5))(_<3))
  println(List.setHead(List(1,2,3,4,5))(8))
  println(List.init(List(1,2,3,4,5)))
  println(List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)))
  println(List.length(List(1,2,3)))
  println(List.sum3(List(1,2,3)))
  println(List.product3(List(1,2,3)))
  println(List.length3(List(1,2,3)))
  println(List.reverse(List(1,2,3)))
  println(List.append(List(1,2,3), List(4,5)))
  println(List.append2(List(1,2,3), List(4,5)))
  println(List.addOne(List(1,2,3)))
  println(List.mkString(List(1,2,3)))
  println(List.filter(List(1,2,3,4,5))(_ % 2 == 0))
  println(List.flatMap(List(1,2,3,4,5))(i => List(i, i)))
  println(List.filter2(List(1,2,3,4,5))(_ % 2 == 0))
}

object List { // `List` companion object
  def sum(ints: List[Int]): Int = ints match { // Pattern matching example
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  } 
  
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
  
  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  
  val example = Cons(1, Cons(2, Cons(3, Nil))) // Creating lists
  val example2 = List(1,2,3)
  val total = sum(example)

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

  def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    l match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  
  def sum2(l: List[Int]) = 
    foldRight(l, 0.0)(_ + _)
  
  def product2(l: List[Double]) = 
    foldRight(l, 1.0)(_ * _)


  def tail[A](l: List[A]): List[A] = 
    l match {
      case Nil => Nil // must throw an exception ?
      case Cons(x, xs) => xs
    }

  def drop[A](l: List[A], n: Int): List[A] = 
    if (n == 0) l else drop(tail(l), n-1)

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = 
    l match {
      case Nil => Nil // must throw an exception ?
      case Cons(x, xs) => if (f(x)) dropWhile(xs)(f) else xs
    }

  def setHead[A](l: List[A])(h: A): List[A] = 
    Cons(h, tail(l))

  def init[A](l: List[A]): List[A] = 
      l match {
        case Nil => Nil // must throw an exception ?
        case Cons(x, Nil) => Nil
        case Cons(x, xs) => Cons(x, init(xs)) 
      }

  //todo exercice 8 : ???
  //ex9 : laisse la liste inchangée, ie permet de construire la liste

  def length[A](l: List[A]): Int = foldRight(l, 0)((a,b) => 1 + b)

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  
  def sum3(l: List[Int]) = 
    foldLeft(l, 0)(_ + _)
  
  def product3(l: List[Double]) = 
    foldLeft(l, 1.0)(_ * _)
  
  def length3[A](l: List[A]) = 
    foldLeft(l, 0)((x, y) => 1 + x)

  def reverse[A](l: List[A]) : List[A] = 
    foldLeft(l, Nil:List[A])((x, y) => Cons(y, x))

  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)(Cons(_,_)) 
  
  def addOne(l: List[Int]): List[Int] = 
    l match {
      case Nil => Nil
      case Cons(x, xs) => Cons(x+1, addOne(xs))
    }
  
  def mkString(l: List[Double]): List[String] = 
    l match {
      case Nil => Nil
      case Cons(x, xs) => Cons(x.toString, mkString(xs))
    }
    
  def map[A,B](l: List[A])(f: A => B): List[B] = 
    l match {
      case Nil => Nil
      case Cons(x, xs) => Cons(f(x), map(xs)(f))
    }
  
  def filter[A](l: List[A])(f: A => Boolean) : List[A] =
    l match {
      case Nil => Nil
      case Cons(x, xs) => if (f(x)) Cons(x, filter(xs)(f)) else filter(xs)(f)
    }
  
  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = 
    l match {
      case Nil => Nil
      case Cons(x, xs) => List.append(f(x), flatMap(xs)(f))
    }
  
  def filter2[A](l: List[A])(f: A => Boolean) : List[A] =
    flatMap(l)(x => if (f(x)) List(x) else Nil)

  //def zip(l1: List[Int], l2: List[Int]) : List[Int] = {

}
