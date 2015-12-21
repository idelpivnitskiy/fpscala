package fpinscala.errorhandling

//hide std library `Either` since we are writing our own in this chapter
import scala.{Either => _}

sealed trait Either[+E, +A] {

  // Exercise 4.6: Implement `map`
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(a) => Right(f(a))
    case Left(e)  => Left(e)
  }
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object EitherTest {

  import fpinscala.util.TestUtils._

  def testMap() = {
    println("--- testMap ---")
    val f = (v: Int) => v + "_" + v
    printAndCheck(Right("1_1"), Right(1).map(f))
    printAndCheck(Left(),       Left().map(f))
    println("---------------\n")
  }

  def main(args: Array[String]) = {
    testMap()
  }
}
