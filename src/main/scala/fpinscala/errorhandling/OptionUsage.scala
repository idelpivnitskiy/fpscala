package fpinscala.errorhandling

//hide std library `Option` and `Some`, since we are writing our own in this chapter
import fpinscala.util.TestUtils._

import scala.{Option => _, Some => _}

object OptionUsage {

  // Exercise 4.2: Implement `variance`
  def variance(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else {
      val mean = xs.sum / xs.length
      val ms = xs.map(d => math.pow(d - mean, 2))
      Some(ms.sum / ms.length)
    }
  }

  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }

  def varianceViaMean(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(m => mean(xs.map(d => math.pow(d - m, 2))))
  }

  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

  // Exercise 4.3: Implement `map2` that combines two Option values using a binary function
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (Some(av), Some(bv)) => Some(f(av, bv))
    case _ => None
  }
}

object OptionUsageTest {

  import OptionUsage._

  def testVariance() = {
    println("--- testVariance ---")
    printAndCheck(None,          variance(Seq()))
    printAndCheck(Some(2.0/3.0), variance(Seq(1, 2, 3)))

    printAndCheck(None,          varianceViaMean(Seq()))
    printAndCheck(Some(2.0/3.0), varianceViaMean(Seq(1, 2, 3)))
    println("--------------------\n")
  }

  def testMap2() = {
    println("--- testMap2 ---")
    val f = (a: Int, b: Int) => a + b
    printAndCheck(None,    map2(None,    None)   (f))
    printAndCheck(None,    map2(None,    Some(2))(f))
    printAndCheck(None,    map2(Some(1), None)   (f))
    printAndCheck(Some(3), map2(Some(1), Some(2))(f))
    println("----------------\n")
  }

  def main(args: Array[String]) = {
    testVariance()
    testMap2()
  }
}
