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

  // Exercise 4.1: Implement `orElse`
  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    map(Some(_)).getOrElse(ob)
  }
  def orElseOpt[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }

  // Exercise 4.1: Implement `filter`
  def filter(f: A => Boolean): Option[A] = {
    if (map(f).getOrElse(false)) this
    else None
  }
  def filterAlt(f: A => Boolean): Option[A] = {
    flatMap(a => if (f(a)) Some(a) else None)
  }
  def filterOpt(f: A => Boolean): Option[A] = this match {
    case Some(v) if f(v) => this
    case _ => None
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

  def testOrElse() = {
    println("--- testOrElse ---")
    printAndCheck(Some(1), Some(1).orElse(None))
    printAndCheck(Some(2), None.orElse(Some(2)))

    printAndCheck(Some(1), Some(1).orElseOpt(None))
    printAndCheck(Some(2), None.orElseOpt(Some(2)))
    println("------------------\n")
  }

  def testFilter() = {
    println("--- testFilter ---")
    val f = (v: Int) => v > 0
    printAndCheck(Some(1), Some(1).filter(f))
    printAndCheck(None,    Some(0).filter(f))
    printAndCheck(None,    None.filter(f))

    printAndCheck(Some(1), Some(1).filterAlt(f))
    printAndCheck(None,    Some(0).filterAlt(f))
    printAndCheck(None,    None.filterAlt(f))

    printAndCheck(Some(1), Some(1).filterOpt(f))
    printAndCheck(None,    Some(0).filterOpt(f))
    printAndCheck(None,    None.filterOpt(f))
    println("------------------\n")
  }

  def main(args: Array[String]) = {
    testMap()
    testGetOrElse()
    testFlatMap()
    testOrElse()
    testFilter()
  }
}
