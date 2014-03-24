package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We will later define other functions in terms of `nextInt`.
}

object RNG {
  def simple(seed: Long): RNG = new RNG {
    def nextInt = {
      val seed2 = (seed*0x5DEECE66DL + 0xBL) & // `&` is bitwise AND
                  ((1L << 48) - 1) // `<<` is left binary shift
      ((seed2 >>> 16).asInstanceOf[Int], // `>>>` is right binary shift with zero fill
       simple(seed2))
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

  def positiveInt(rng: RNG): (Int, RNG) = {
    val p = rng.nextInt
    if (p._1 == Int.MinValue) positiveInt(p._2) else (p._1.abs, p._2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val p = positiveInt(rng)
    (p._1.toDouble/Int.MaxValue, p._2)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val p1 = rng.nextInt
    val p2 = double(p1._2)
    ((p1._1, p2._1), p2._2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val p1 = double(rng) 
    val p2 = p1._2.nextInt
    ((p1._1, p2._1), p2._2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val p1 = double(rng) 
    val p2 = double(p1._2) 
    val p3 = double(p2._2) 
    ((p1._1, p2._1, p3._1), p3._2)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count==0) (Nil, rng) else {
      val p=positiveInt(rng)
      (p._1 :: ints(count-1)(p._2)._1, p._2)
    }
  }

  def positiveMax(n: Int): Rand[Int] = {
    RNG.map(int){(i: Int) => ((i.toDouble/Int.MaxValue)*n).toInt}
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a,b), rng2)
    }
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = sys.error("todo")

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }
  }

  // using flatMap
  def positiveInt2(rng: RNG): (Int, RNG) = {
    def g = {rng: RNG => positiveInt2(rng)}
    RNG.flatMap[Int,Int](int)({(i:Int) => if (i == Int.MinValue) g else unit(i)})(rng)
  }

  //map with flatMap
  def mapf[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(f andThen unit)

  // map2 with flatMap
  def map2f[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = ???
}

object Test extends App {
  val t=RNG.simple(1)
  println(t.nextInt)
  println(RNG.positiveInt(t))
  println(RNG.double(t))
  println(RNG.intDouble(t))
  println(RNG.doubleInt(t))
  println(RNG.double3(t))
  println(RNG.ints(4)(t))
  println(RNG.positiveMax(5000)(t.nextInt._2))
  println(RNG.positiveInt2(t))
  
  val m = Machine(true, 2, 10) 
  println(State.simulateMachine(List(Coin)).run(Machine(true, 2, 10)))
  println(State.simulateMachine(List(Coin)).run(Machine(false, 2, 10)))
  println(State.simulateMachine(List(Turn)).run(Machine(true, 2, 10)))
  println(State.simulateMachine(List(Turn)).run(Machine(false, 2, 10)))
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    State((s : S) => {
      val (a, s2) = run(s)
      (f(a), s2)
    })

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    State((s: S) => {
      val (a, s2) = run(s)
      val (b, s3) = sb.run(s2)
      (f(a,b), s3)
    })

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State((s: S) => {
      val (a, s2) = run(s)
      f(a).run(s2)
    })
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, Int] = 
    State(
    (m : Machine) => inputs.head match {
      case Coin => val Machine(l, ca, co) = m
        if (ca > 0 && l) (co+1, Machine(false, ca, co+1))
        else (co, m)
      case Turn =>
       val Machine(l, ca, co) = m
       if (ca > 0 && !l) (co, Machine(true, ca-1, co))
       else (co, m)
   })
}
