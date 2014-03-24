package fpinscala.errorhandling

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }
  
  def getOrElse[B>:A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }
  
  def flatMap[B](f: A => Option[B]): Option[B] = 
    map(f) getOrElse None
  
  /*
  Of course, we can also implement `flatMap` with explicit pattern matching.
  */
  def flatMap_1[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }
  
  def orElse[B>:A](ob: => Option[B]): Option[B] = 
    this map (Some(_)) getOrElse ob
  
  /*
  Again, we can implement this with explicit pattern matching. 
  */
  def orElse_1[B>:A](ob: => Option[B]): Option[B] = this match {
    case None => ob 
    case _ => this
  }
  
  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
  }
  /*
  This can also be defined in terms of `flatMap`.
  */
  def filter_1(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) Some(a) else None)
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val x: Int = throw new Exception("fail!")
    try {
      val y = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  import java.util.regex._
  
  def pattern(s: String): Option[Pattern] =
    try {
      Some(Pattern.compile(s))
    } catch {
      case e: PatternSyntaxException => None
    }

  def mkMatcher(pat: String): Option[String => Boolean] = 
    pattern(pat) map (p => (s: String) => p.matcher(s).matches) // The details of this API don't matter too much, but `p.matcher(s).matches` will check if the string `s` matches the pattern `p`.

  def mkMatcher_1(pat: String): Option[String => Boolean] = 
    for {
      p <- pattern(pat)
    } yield ((s: String) => p.matcher(s).matches)
  
  def doesMatch(pat: String, s: String): Option[Boolean] = 
    for {
      p <- mkMatcher_1(pat)
    } yield p(s)

  def bothMatch(pat: String, pat2: String, s: String): Option[Boolean] =
    for {
      f <- mkMatcher(pat)
      g <- mkMatcher(pat2)
    } yield f(s) && g(s)

  def bothMatch_1(pat: String, pat2: String, s: String): Option[Boolean] =
    mkMatcher(pat) flatMap (f => 
    mkMatcher(pat2) map     (g => 
    f(s) && g(s)))
  def variance(xs: Seq[Double]): Option[Double] = sys.error("todo")

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    for { 
      aa <- a
      bb <-b 
    } yield f(aa, bb)
    }

  def bothMatch_2(pat1: String, pat2: String, s: String): Option[Boolean] = {
    map2(mkMatcher(pat1), mkMatcher(pat2))((a, b) => a(s) && b(s))
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    //a.foldRight(Some(Nil.asInstanceOf[List[A]]))((q,b)=>q.flatMap(aa => b.map(bb=>bb::aa)))
    a match {
      case h::t => h.flatMap(hh => sequence(t).map(tt=>hh::tt))
      case Nil => Some(Nil)
    }
  } 

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    sequence(a.map(f(_)))
  }

  def traverse_1[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a.foldRight(Some(Nil).asInstanceOf[Option[List[B]]])(((x,y) => f(x).flatMap(xx => y.map(xx :: _))))
  }
  def sequence_2[A](a: List[Option[A]]): Option[List[A]] = {
    traverse(a)(a=>a)
  }
}

object OptionRun extends App {
  def f(s: String, n: Int) = s.substring(n)
  var a=Some("Hello World")
  var b=Some(3)
  println(Option.map2(a,b)(f))

  println(Option.bothMatch_2("H*", "G*", "Hello World"))
  
  val l = List(Some(1), Some(2), Some(3))
  println(Option.sequence(l))
  
  val l2 = List("A", "AA", "AAA")
  val f = ((s:String) => Some(s.length))

  println(Option.traverse(l2)(f))

  println(Option.traverse_1(l2)(f))
  
  println(Option.sequence_2(l))
}
