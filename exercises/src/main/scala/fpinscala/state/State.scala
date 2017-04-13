package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, rng2) = rng.nextInt
    if (i == Int.MinValue) nonNegativeInt(rng2) else (Math.abs(i), rng2)
  }


  def double(rng: RNG): (Double, RNG) = {
    val (i, rng2) = rng.nextInt
    (i.toDouble/Int.MaxValue, rng2)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i,d), rng3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (d, rng2) = double(rng)
    val (i, rng3) = rng2.nextInt
    ((d, i), rng3)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1,d2,d3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 0) (Nil, rng)
    else {
      val (i, rng2) = rng.nextInt
      (i :: ints(count - 1)(rng2)._1, rng2)
    }
  }

  def double2(rng: RNG): (Double, RNG) = map(int)(_.toDouble/Int.MaxValue)(rng)

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng)
      (f(a,b), rng3)
    }
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    rng => {
      fs match {
        case Nil => (Nil, rng)
        case x :: xs => val (a, rng2) = x(rng)
          (a :: sequence(xs)(rng2)._1, rng2)
      }
    }
  }

  def ints2(count: Int)(rng: RNG): (List[Int], RNG) = sequence(List.fill(count)(int))(rng)

  def nonNegativeLessThan(n: Int) : Rand[Int] = {
    rng =>
      val (i, rng2) = nonNegativeInt(rng)
      val mod = i % n
      if (i+(n-1)-mod>=0)
        (mod, rng2)
      else nonNegativeLessThan(n)(rng)
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }
  }

  def nonNegativeLessThan_(n: Int) : Rand[Int] = {
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod)
      else nonNegativeLessThan_(n)
    }
  }

  def map_[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => rng => (f(a), rng))

  def map2_[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)(a => map(rb)(b => f(a, b)))



}

object Test extends App {
  import RNG._
  println(double3(Simple(45)))
  println(ints(5)(Simple(45)))
  println(ints2(5)(Simple(45)))
  println(nonNegativeLessThan(1)(Simple(45)))
  println(nonNegativeLessThan(2)(Simple(45)))
  println(nonNegativeLessThan(3)(Simple(45)))
  println(nonNegativeLessThan(4)(Simple(45)))
  println(nonNegativeLessThan(5)(Simple(45)))
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = State { s =>
    val (a, s2) = run(s)
    (f(a), s2)
  }
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = State { s =>
    val (a, s2) = run(s)
    val (b, s3) = sb.run(s2)
    (f(a,b), s3)
  }
  def flatMap[B](f: A => State[S, B]): State[S, B] = State { s =>
    val (a, s2) = run(s)
    f(a).run(s2)
  }
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def unit[A, S](a: A): State[S, A] =
    State { s => (a, s) }

  def sequence[A, S](fs: List[State[S, A]]): State[S, List[A]] = {
    State { rng => {
      fs match {
        case Nil => (Nil, rng)
        case x :: xs => val (a, rng2) = x.run(rng)
          (a :: sequence(xs).run(rng2)._1, rng2)
      }
    }}

  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    val ls : List[State[Machine, Unit]] = transform(inputs).map(f => State {m: Machine =>((), f(m))})
    val ls2 : State[Machine, List[Unit]] = sequence(ls)
    State { s =>
      val (_, m) = ls2.run(s)
      ((m.candies, m.coins), m)
    }
  }

  def insertCoin : Machine => Machine = m => m match {
    case Machine(_, 0, _) => m
    case Machine(true, candies, coins) => Machine(false, candies, coins+1)
    case Machine(false, candies, coins) => m
  }

  def turn : Machine => Machine = m => m match {
    case Machine(_, 0, _) => m
    case Machine(false, candies, coins) => Machine(true, candies - 1, coins)
    case Machine(true, candies, coins) => m
  }

  def transform(inputs: List[Input]) : List[Machine => Machine] = inputs.map(_ match {
    case Coin => insertCoin
    case Turn => turn
  }
  )
}

object SimTest extends App {
  import State._
  val m = Machine(true, 10, 5)
  val m2 = insertCoin(m)
  val m3 = turn(m2)
  println(m3)

  println(simulateMachine(List(Coin, Turn)).run(m))
}