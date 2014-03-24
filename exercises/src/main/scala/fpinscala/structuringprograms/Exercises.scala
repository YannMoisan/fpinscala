package fpinscala.structuringprograms

case class Box(height: Double, width: Double)

object Exercises extends App {

  println("dd")
  val b1 = Box(2,2)
  val b2 = Box(1,3)
  println(wider(b1,b2))
  println(taller(b1,b2))


  def greaterBy(x: Box, y: Box, f: Box => Double): Box = 
  if (f(x) > f(y)) x else y

  def wider(x: Box, y: Box): Box =
    greaterBy(x, y, _.width)
  
  println(2)
   println(2)
   def taller(x: Box, y: Box) =
    greaterBy(x, y, _.height)

  type Pred[A] = A => Boolean

  // TODO see how to call abs in a polymorphic context
  //def absolute[A](f: A => A): A => A = i => math.abs(f(i))
  //def g = absolute[Int](i=>i-10)
  //println(g(7))

  def divisibleBy(k: Int): Pred[Int] = i => i % k == 0
  def d = divisibleBy(3)
  println("5 divisibleBy 3 : " + d(5))
  println("6 divisibleBy 3 : " + d(6))
  
  def even(n: Int): Boolean = divisibleBy(2)(n)
  println("5 even : " + even(5))
  println("6 even : " + even(6))

  def and(n: Int) = divisibleBy(3)(n) && divisibleBy(5)(n)
  println("45 and : " + and(45))
  println("20 and : " + and(20))

  def or(n: Int) = divisibleBy(3)(n) || divisibleBy(5)(n)
  println("45 or : " + or(45))
  println("20 or : " + or(20))

  def lift[A](f: (Boolean, Boolean) => Boolean,
              g: Pred[A],
              h: Pred[A]): Pred[A] = i => f(g(i), h(i))

  def andlift(n: Int) = lift[Int](_ && _, divisibleBy(3), divisibleBy(5))(n)
  println("45 andlift : " + andlift(45))
  println("20 andlift : " + andlift(20))

  def orlift(n: Int) = lift[Int](_ || _, divisibleBy(3), divisibleBy(5))(n)
  println("45 orlift : " + orlift(45))
  println("20 orlift : " + orlift(20))

  def curry[A,B,C](f: (A, B) => C): A => B => C = (a => b => f(a, b))
  println(curry[Int, Int, Int](_ + _)(2)(4))

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)
  println(uncurry[Int, Int, Int](x => y => x+y)(2,4))

  def compose[A,B,C](f: B => C, g: A => B): A => C = a => f(g(a))
  println(compose[String, Int, String](_.toString, _.length)("fpinscala"))

  def lift3[A,B,C,D,E](f: (B, C, D) => E)(g: A => B,
                                          h: A => C,
                                            i: A => D): A => E = a => f(g(a), h(a), i(a))

  def fib(n: Int): Int = if (n < 2) n else fib(n-1) + fib(n-2)
  println("fib 0:"+fib(0))
  println("fib 1:"+fib(1))
  println("fib 2:"+fib(2))
  println("fib 3:"+fib(3))
  println("fib 4:"+fib(4))
  println("fib 5:"+fib(5))

  def fib2(n:Int) : Int = {
    def loop(n:Int, acc1:Int, acc2:Int):Int = {
      if (n<2) acc1 else loop(n-1, acc1+acc2, acc1)  
    }

    loop(n, 1, 0)
  }
  println("fib2 0:"+fib2(0))
  println("fib2 1:"+fib2(1))
  println("fib2 2:"+fib2(2))
  println("fib2 3:"+fib2(3))
  println("fib2 4:"+fib2(4))
  println("fib2 5:"+fib2(5))

  def sqrt(n: Double): Double = {
    def f(x: Double) = (x * x) - n // We want to find the `x` such that `x` squared minus `n` equals `0`.
    iterateWhile(2.0)(x => x - f(x) / (2 * x), // Starting with a guess of `2.0`, iteratively improve the guess.
                      x => f(x).abs > 1e-14) // `1e-14` is a way of writing `10` to the `-14`th power, a rather small number. When the difference between the guess and the answer is smaller than this, the guess is "good enough".
  }
  
  def iterateWhile[A](a: A)(f: A => A, p: Pred[A]): A = sys.error("todo")
}
