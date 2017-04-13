package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

//trait Prop {
//  def check : Boolean
//  def &&(prop: Prop) : Prop = new Prop() {
//    override def check: Boolean = check && prop.check
//  }
//}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(p: Prop) : Prop = {
    Prop {
      (m, n, rng) =>
        val r1 = this.run(m, n, rng)
        val r2 = p.run(m, n, rng)
        (r1, r2) match {
          case (Passed, Passed) => Passed
          case (Passed, Falsified(f,s)) => Falsified(f,s)
          case (Falsified(f,s), Passed) => Falsified(f,s)
          case (Falsified(f1,s1), Falsified(f2,s2)) => Falsified(f1+f2, s1+s2)
        }
    }
  }
  def ||(p: Prop) : Prop = {
    Prop {
      (m, n, rng) =>
        val r1 = this.run(m, n, rng)
        val r2 = p.run(m, n, rng)
        (r1, r2) match {
          case (Passed, Passed) => Passed
          case (Passed, Falsified(f,s)) => Passed
          case (Falsified(f,s), Passed) => Passed
          case (Falsified(f1,s1), Falsified(f2,s2)) => Falsified(f1+f2, s1+s2)
        }
    }
  }
}

object Prop {
  type MaxSize = Int
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int

  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    override def isFalsified: Boolean = false
  }
  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    override def isFalsified: Boolean = true
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (m, n,rng) => randomStream(as)(rng).zipWith(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max,n,rng) =>
      val casesPerSize = (n - 1) / max + 1
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, n, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max,n,rng)
  }

  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
//      case Proved =>
//        println(s"+ OK, proved property.")
    }



  def buildMsg[A](s:A, e:Exception) : String = {
    s"testcase $s\n" +
    s"generated an exception : ${e.getMessage}\n" +
    s"stack trace\n ${e.getStackTrace.mkString("\n")}"
  }

  def randomStream[A](g: Gen[A])(rng:RNG) : Stream[A] = Stream.unfold(rng)(rng=>Some(g.sample.run(rng)))
}

object Gen {
  def choose(start: Int, stopExclusive: Int) : Gen[Int] = Gen(State(RNG.nonNegativeLessThan(stopExclusive-start)).map(_+start))
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))
  def boolean: Gen[Boolean] = Gen(choose(0, 2).sample.map(i => i==0))
  def listOfN[A](n: Int, g: Gen[A]) : Gen[List[A]] = {
    val gens : List[State[RNG, A]] = (1 to n).toList.map(_ => g.sample)
    Gen(State.sequence(gens))
  }

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
    boolean.flatMap(b => if (b) g1 else g2)
  }

  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => g.listOfN(n))
}


case class Gen[A](sample: State[RNG, A]) {
  def map[A, B](f: A => B): Gen[B] = ???

  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    val s: State[RNG, B] = sample.flatMap(x => f(x).sample)
    Gen(s)
  }

  /* A method alias for the function we wrote earlier. */
  def listOfN(size: Int): Gen[List[A]] =
    Gen.listOfN(size, this)

  def listOfN(size: Gen[Int]): Gen[List[A]] = {
    size.flatMap(i => Gen.listOfN(i, this))
  }

  def listOf[A](g:Gen[A]): SGen[List[A]] = {
    SGen { i => g.listOfN(unit(i))}
  }

  def unsized: SGen[A] = SGen[A](_ => this)

}

//trait Gen[A] {
//  def map[A,B](f: A => B): Gen[B] = ???
//  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
//}

//trait SGen[+A] {
//
//}

case class SGen[A](forSize : Int => Gen[A]) {
  def apply(n: Int): Gen[A] = forSize(n)

  def map[B](f: A=> B) : SGen[B] = SGen(i => forSize(i).map(f))
  def flatMap[B](f: A=> SGen[B]) : SGen[B] = SGen{{i:Int  => forSize(i).flatMap(a=>f(a).forSize(i))}}

}

object Test extends App {
  import Prop._
  import Gen._
  println(choose(1,3).sample.run(RNG.Simple(12)))
  println(listOfN(20, choose(1,7)).sample.run(RNG.Simple(12)))
  println(listOfN(20, boolean).sample.run(RNG.Simple(12)))

  val smallInt = Gen.choose(-10, 10)
  val maxProp = forAll(listOf(smallInt)) {
    ns =>
      val max = ns.max
      !ns.exists(_ > max)
  }
  run(maxProp)
}

