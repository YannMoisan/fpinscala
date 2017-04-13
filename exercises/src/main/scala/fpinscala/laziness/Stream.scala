package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  def take(n: Int): Stream[A] = {
    def take_(s: Stream[A], n: Int) : Stream[A] = if (n == 0) Empty else {
      s match {
        case Empty => Empty
        case Cons(h, t) => Cons(h, () => take_(t(), n - 1))
      }
    }
    take_(this, n)
  }

  def drop(n: Int): Stream[A] = {
    def drop_(s: Stream[A], n: Int) : Stream[A] = if (n == 0) s else {
      s match {
        case Empty => Empty
        case Cons(h, t) => drop_(t(), n - 1)
      }
    }
    drop_(this, n)
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    def take_(s: Stream[A]): Stream[A] =
      s match {
        case Cons(h, t) if p(h()) => Cons(h, () => take_(t()))
        case _ => Empty
      }
    take_(this)
  }

  def forAll(p: A => Boolean): Boolean = {
    def forAll_(s: Stream[A], p: A => Boolean): Boolean = {
      s match {
        case Empty => true
        case Cons(h, t) => if (p(h())) forAll_(t(), p) else false
      }
    }
    forAll_(this, p)
  }

  // 5.13 implement with unfold
  def map[B](f: A => B): Stream[B] = {
    Stream.unfold(this) {
      case Cons(h, t) => Some(f(h()), t())
      case Empty => None
    }
  }

  def take_(n: Int): Stream[A] = {
    Stream.unfold((this, n)) {
      case (Cons(h, t), n) if n > 0 => Some(h(), (t(), n - 1))
      case _ => None
    }
  }

  def zipWith[B](b: Stream[B]): Stream[(A, B)] = {
    unfold((this, b))(t => t match {
      case (Cons(ha, ta), Cons(hb, tb)) => Some(((ha(), hb()), (ta(), tb())))
      case _ => None
    })
  }

  //
  def takeWhile2(p: A => Boolean): Stream[A] = {
    foldRight(Empty: Stream[A])((a, b) => if (p(a)) Cons(() => a, () => b) else Empty)
  }

  def startsWith[B](s: Stream[B]): Boolean = {
    this match {
      case Cons(h, t) => s match {
        case Cons(hs, ts) => if (h() == hs()) t().startsWith(ts()) else false
        case Empty => true
      }
      case Empty => s match {
        case Cons(_,_) => false
        case Empty => true
      }
    }
  }

  def toList : List[A] = foldRight(Nil:List[A])(_ :: _)
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  def from(n: Int): Stream[Int] = Stream.cons(n, from(n+1))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z).map{case (a, z2) => Stream.cons(a, unfold(z2)(f))}.getOrElse(Empty)
  }

}

object Test extends App {
  println(Stream(1,2,3).take(2))
  println(Stream(1,2,3).toList)

  println(Stream(1,2,3).take(2).toList)
  println(Stream(1,2,3).drop(2).toList)

  println(Stream(1,2,3).takeWhile(_ <= 2).toList)

  println(Stream(1,2,3).forAll(_ <= 2))
  println(Stream(1,2,3).forAll(_ <= 4))

  println(Stream.unfold(1)(i=> if (i<10) Some(i, i+1) else None).take(20).toList)

  val s = Stream.unfold(1)(i=> if (i<10) Some(i, i+1) else None)

  println(s.zipWith(s).take(20).toList)

  println(Stream(1,2,3).startsWith(Stream(1,2)))
  println(Stream(1,2,3).startsWith(Stream(1,2,3)))
  println(Stream(1,2,3).startsWith(Stream(1,2,3,4)))
  println(Stream(1,2,3).startsWith(Stream(1,3)))

}