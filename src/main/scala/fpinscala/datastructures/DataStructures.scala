package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

// `List` companion object. Contains functions for creating and working with lists.
object List {

  def sum(ints: List[Int]): Int = ints match {
    // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // Exercise 3.2: Implement `tail` function with O(n) time complexity
  def tail[A](l: List[A]): List[A] = l match {
    case Cons(_, t) => t
    case _ => throw new IllegalArgumentException("list can not be empty")
  }

  // Exercise 3.3: Implement `setHead` function with O(n) time complexity
  // for replacing the first element of a List with different value
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Cons(_, t) => Cons(h, t)
    case _ => throw new IllegalArgumentException("list can not be empty")
  }

  // Exercise 3.4: Implement `drop` function, which removes the first n elements from a list
  def drop[A](l: List[A], n: Int): List[A] = {
    require(n >= 0)
    if (n == 0) l
    else l match {
      case Cons(_, t) => drop(t, n - 1)
      case _ => l
    }
  }

  // Exercise 3.5: Implement `dropWhile`, which removes elements from the List prefix
  // as long as they match a predicate
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  // Exercise 3.6: Implement `init`, that returns a List consisting of all but the last element of the List
  def init[A](l: List[A]): List[A] = l match {
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
    case _ => l
  }

  def foldRight[A, B](as: List[A], v: B)(f: (A, B) => B): B = as match {
    case Nil => v
    case Cons(x, xs) => f(x, foldRight(xs, v)(f))
  }

  def sum2(as: List[Int]): Int = {
    foldRight(as, 0)(_ + _)
  }

  def product2(as: List[Double]): Double = {
    foldRight(as, 1.0)(_ * _)
  }

  // Exercise 3.8: What will happen in this case?
  // My assumption: this function will return the same result
  val ex38 = foldRight(List(1, 2, 3), Nil:List[Int])(Cons(_,_))

  // Exercise 3.9: Implement `length` using `foldRight`
  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((_, acc) => 1 + acc)
  }
}

object DataStructures {

  import List._

  // Exercise 3.1: Call `patternMatching` function with different arguments to match all cases
  def patternMatching(l: List[Int]): Int = l match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }
}

object DataStructuresTest {

  import DataStructures._
  import List._

  def testPatternMatching() = {
    println("--- testEx1 ---")
    printAndCheck(1,   patternMatching(List(1, 2, 4, 5)))
    printAndCheck(42,  patternMatching(Nil))
    printAndCheck(42,  patternMatching(List()))
    printAndCheck(3,   patternMatching(List(1, 2, 3, 4, 5)))
    printAndCheck(12,  patternMatching(List(3, 4, 5)))
    printAndCheck(101, patternMatching(null))
    println("---------------\n")
  }

  def testTail() = {
    println("--- testTail ---")
    printAndCheck(List(2, 3), tail(List(1, 2, 3)))
    printAndCheck(Nil, tail(List(1)))
    try {
      printAndCheck(Nil, tail(Nil))
    } catch {
      case e: IllegalArgumentException => println(e.getMessage)
    }
    try {
      printAndCheck(List(2, 3), tail(null))
    } catch {
      case e: IllegalArgumentException => println(e.getMessage)
    }
    println("----------------\n")
  }

  def testSetHead() = {
    println("--- testSetHead ---")
    printAndCheck(List(2, 2), setHead(List(1, 2), 2))
    printAndCheck(List(2), setHead(List(1), 2))
    try {
      printAndCheck(List(2), setHead(Nil, 2))
    } catch {
      case e: IllegalArgumentException => println(e.getMessage)
    }
    try {
      printAndCheck(List(2), setHead(null, 2))
    } catch {
      case e: IllegalArgumentException => println(e.getMessage)
    }
    println("-------------------\n")
  }

  def testDrop() = {
    println("--- testDrop ---")
    printAndCheck(List(3), drop(List(1, 2, 3), 2))
    printAndCheck(List(1), drop(List(1), 0))
    printAndCheck(Nil, drop(List(1, 2, 3), 3))
    printAndCheck(Nil, drop(Nil, 2))
    printAndCheck(null, drop(null, 2))
    try {
      printAndCheck(List(1), drop(List(1), -5))
    } catch {
      case e: IllegalArgumentException => println(e.getMessage)
    }
    println("----------------\n")
  }

  def testDropWhile() = {
    println("--- testDropWhile ---")
    printAndCheck(List(3), dropWhile(List(1, 2, 3), (x: Int) => x < 3))
    printAndCheck(List(1, 2, 3), dropWhile(List(1, 2, 3), (x: Int) => x > 3))
    printAndCheck(Nil, dropWhile(List(1, 2, 3), (x: Int) => x < 5))
    printAndCheck(Nil, dropWhile(Nil, (x: Int) => x < 5))
    printAndCheck(null, dropWhile(null, (x: Int) => x < 5))
    println("---------------------\n")
  }

  def testInit() = {
    println("--- testInit ---")
    printAndCheck(List(1, 2, 3), init(List(1, 2, 3, 4)))
    printAndCheck(Nil, init(List(1)))
    printAndCheck(Nil, init(Nil))
    printAndCheck(null, init(null))
    println("----------------\n")
  }

  def testEx3_8() = {
    println("--- testEx3_8 ---")
    printAndCheck(List(1, 2, 3), ex38)
    println("-----------------\n")
  }

  def testLength() = {
    println("--- testLength ---")
    printAndCheck(3, length(List(1, 2, 3)))
    printAndCheck(1, length(List(1)))
    printAndCheck(0, length(Nil))
    println("------------------\n")
  }

  private def printAndCheck[A](expected: A, actual: A) = {
    println("%s == %s".format(expected, actual))
    require(expected == actual)
  }

  def main(args: Array[String]) = {
    testPatternMatching()
    testTail()
    testSetHead()
    testDrop()
    testDropWhile()
    testInit()
    testEx3_8()
    testLength()
  }
}
