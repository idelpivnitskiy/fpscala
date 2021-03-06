package fpinscala.errorhandling

import scala.{Either => _}

object EitherUsage {

  // Exercise 4.7: Implement `sequence`
  def sequence[E, A](l: List[Either[E, A]]): Either[E, List[A]] = {
    l.foldRight[Either[E, List[A]]](Right(Nil))((h, t) => h.map2(t)(_ :: _))
  }

  // Exercise 4.7: Implement `traverse`
  def traverse[E, A, B](l: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    l.foldRight[Either[E, List[B]]](Right(Nil))((h, t) => f(h).map2(t)(_ :: _))
  }
  def sequenceViaTraverse[E, A](l: List[Either[E, A]]): Either[E, List[A]] = {
    traverse(l)(x => x)
  }
}

object EitherUsageTest {

  import EitherUsage._
  import fpinscala.util.TestUtils._

  def testSequence() = {
    println("--- testSequence ---")
    def test(f: List[Either[String, Int]] => Either[String, List[Int]]) = {
      printAndCheck(Right(List(1, 2, 3)), f(List(Right(1), Right(2), Right(3))))
      printAndCheck(Right(List(1)),       f(List(Right(1))))
      printAndCheck(Left("empty"),        f(List(Left("empty"), Right(2),      Right(3))))
      printAndCheck(Left("empty"),        f(List(Right(1),      Left("empty"), Right(3))))
      printAndCheck(Left("empty"),        f(List(Right(1),      Right(2),      Left("empty"))))
      printAndCheck(Left("empty"),        f(List(Left("empty"))))
      println()
    }
    test(sequence)
    test(sequenceViaTraverse)
    println("--------------------\n")
  }

  def main(args: Array[String]) = {
    testSequence()
  }
}
