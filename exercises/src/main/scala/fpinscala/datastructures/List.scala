package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

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


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(h, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0) l else drop(List.tail(l), n - 1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] =
    l match {
      case Nil | Cons(_, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, b) => b + 1)

  @tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)(Cons(_, _))

  def addOne(a: List[Int]) : List[Int] = foldRight(a, Nil: List[Int])((a,b) => Cons(a + 1, b))

  def toStr(a: List[Double]) : List[String] = foldRight(a, Nil: List[String])((a,b) => Cons(a.toString, b))

  def map[A,B](l: List[A])(f: A => B): List[B] = foldRight(l, Nil: List[B])((a,b) => Cons(f(a), b))
}

object Test extends App {
  import List._
  println(x)
  println("tail(Nil): "+tail(Nil))
  println("tail(List(1,2,3)): "+tail(List(1,2,3)))

  println(setHead(Nil, 0))
  println(setHead(List(1,2,3), 0))

  println(drop(Nil, 2))
  println(drop(List(1,2,3), 1))
  println(drop(List(1,2,3), 2))
  println(drop(List(1,2,3), 3))
  println(drop(List(1,2,3), 4))

  println(dropWhile(List(1,2,3), (_: Int) <= 2 ))

  println(init(Nil))
  println(init(List(1)))
  println(init(List(1,2)))

  // 3.7 no
  //
  // 3.8 it's the same
  println(foldRight(List(1,2,3), Nil:List[Int])(Cons(_, _)))

  println(length(Nil))
  println(length(List(1,2,3)))

  println(foldLeft(List(1,2,3), Nil:List[Int])((b,a) => Cons(a, b)))

  println(append(List(1,2,3), List(4,5,6)))
  println(append2(List(1,2,3), List(4,5,6)))

  println(addOne(List(1,2,3)))

  println(toStr(List(1,2,3)))


}
