package fpinscala.errorhandling

//hide std library `Option` and `Some`, since we are writing our own in this chapter
import scala.{Option => _, Some => _}

sealed trait Option[+A] {

  // Exercise 4.1: Implement `map`
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(v) => Some(f(v))
  }
}
case object None extends Option[Nothing]
case class Some[+A](get: A) extends Option[A]

object OptionTest {

  import fpinscala.util.TestUtils._

  def testMap() = {
    println("--- testMap ---")
    val f = (v: Int) => v + "_" + v
    printAndCheck(Some("1_1"), Some(1).map(f))
    printAndCheck(None,        None.map(f))
    println("---------------\n")
  }

  def main(args: Array[String]) = {
    testMap()
  }
}
