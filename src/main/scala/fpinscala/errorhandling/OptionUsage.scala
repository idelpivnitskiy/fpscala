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
}

object OptionUsageTest {

  import OptionUsage._

  def testVariance() = {
    println("--- testVariance ---")
    printAndCheck(None,          variance(Seq()))
    printAndCheck(Some(2.0/3.0), variance(Seq(1, 2, 3)))
    println("--------------------\n")
  }

  def main(args: Array[String]) = {
    testVariance()
  }
}
