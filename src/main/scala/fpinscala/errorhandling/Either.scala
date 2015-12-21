package fpinscala.errorhandling

//hide std library `Either` since we are writing our own in this chapter
import scala.{Either => _}

sealed trait Either[+E, +A] {

  // Exercise 4.6: Implement `map`
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(a) => Right(f(a))
    case Left(e)  => Left(e)
  }

  // Exercise 4.6: Implement `flatMap`
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => f(a)
    case Left(e) => Left(e)
  }

  // Exercise 4.6: Implement `orElse`
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(_) => this
    case Left(_) => b
  }

  // Exercise 4.6: Implement `map2`
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = (this, b) match {
    case (Right(av), Right(bv)) => Right(f(av, bv))
    case (Left(e), _) => Left(e)
    case (_, Left(e)) => Left(e)
  }

  def map2ViaYield[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = for {
    a1 <- this
    b1 <- b
  } yield f(a1, b1)
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

  def testFlatMap() = {
    println("--- testFlatMap ---")
    val f = (v: Int) => {
      if (v > 0) Right(v + "_" + v)
      else Left("less or equal to zero")
    }
    printAndCheck(Right("1_1"), Right(1).flatMap(f))
    printAndCheck(Left("less or equal to zero"), Right(0).flatMap(f))
    printAndCheck(Left("my error"), Left("my error").flatMap(f))
    println("-------------------\n")
  }

  def testOrElse() = {
    println("--- testOrElse ---")
    val b = Right(0)
    printAndCheck(Right(1), Right(1).orElse(b))
    printAndCheck(b,        Left().orElse(b))
    println("------------------\n")
  }

  def testMap2() = {
    println("--- testOrElse ---")
    val ar = Right(1)
    val br = Right(2)
    val f = (a: Int, b: Int) => a + b
    val al = Left("error a")
    val bl = Left("error b")
    printAndCheck(Right(3), ar.map2(br)(f))
    printAndCheck(al,       al.map2(br)(f))
    printAndCheck(bl,       ar.map2(bl)(f))
    printAndCheck(al,       al.map2(bl)(f))

    printAndCheck(Right(3), ar.map2ViaYield(br)(f))
    printAndCheck(al,       al.map2ViaYield(br)(f))
    printAndCheck(bl,       ar.map2ViaYield(bl)(f))
    printAndCheck(al,       al.map2ViaYield(bl)(f))
    println("------------------\n")
  }

  def main(args: Array[String]) = {
    testMap()
    testFlatMap()
    testOrElse()
    testMap2()
  }
}
