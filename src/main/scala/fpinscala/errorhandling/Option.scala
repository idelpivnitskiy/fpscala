package fpinscala.errorhandling

//hide std library `Option` and `Some`, since we are writing our own in this chapter
import scala.{Option => _, Some => _}

sealed trait Option[+A] {

  // Exercise 4.1: Implement `map`
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(v) => Some(f(v))
  }

  // Exercise 4.1: Implement `getOrElse`
  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(v) => v
  }

  // Exercise 4.1: Implement `flatMap`
  def flatMap[B](f: A => Option[B]): Option[B] = {
    map(f).getOrElse(None)
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

  def testGetOrElse() = {
    println("--- testMap ---")
    printAndCheck(1, Some(1).getOrElse(2))
    printAndCheck(2, None.getOrElse(2))
    println("---------------\n")
  }

  def testFlatMap() = {
    println("--- testFlatMap ---")
    val f = (v: Int) => if (v > 0) Some(v + "_" + v) else None
    printAndCheck(Some("1_1"), Some(1).flatMap(f))
    printAndCheck(None,        Some(0).flatMap(f))
    printAndCheck(None,        None.flatMap(f))
    println("-------------------\n")
  }

  def main(args: Array[String]) = {
    testMap()
    testGetOrElse()
    testFlatMap()
  }
}
