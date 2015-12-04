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

  // Exercise 3.10: Implement `foldLeft`, which will be tail-recursive
  @annotation.tailrec
  def foldLeft[A, B](l: List[A], v: B)(f: (B, A) => B): B = l match {
    case Nil => v
    case Cons(x, xs) => foldLeft(xs, f(v, x))(f)
  }

  // Exercise 3.11: Write `sum`, `product` and `length` using `foldLeft`
  def sumFL(as: List[Int]): Int =
    foldLeft(as, 0)(_ + _)

  def productFL(as: List[Double]): Double =
    foldLeft(as, 1.0)(_ * _)

  def lengthFL[A](l: List[A]): Int =
    foldLeft(l, 0)((acc, _) => 1 + acc)

  // Exercise 3.12: Implement `reverse`
  def reverse[A](l: List[A]): List[A] = {
    foldLeft(l, Nil:List[A])((b, a) => Cons(a, b))
  }

  // Exercise 3.13: Implement `foldRightViaFoldLeft`
  def foldRightViaFoldLeft[A, B](l: List[A], b: B)(f: (A, B) => B): B = {
    foldLeft(reverse(l), b)((b, a) => f(a, b))
  }

  // Exercise 3.14: Implement `append`
  def append[A](l: List[A], e: A): List[A] =
    foldRight(l, Cons(e, Nil))(Cons(_,_))

  // Exercise 3.15: Implement `concatLists`
  def concatLists[A](ls: List[List[A]]): List[A] = {
    foldRight(ls, Nil:List[A])(appendList)
  }

  def appendList[A](first: List[A], second: List[A]): List[A] = {
    foldRight(first, second)(Cons(_,_))
  }

  // Exercise 3.16: Implement `addOne`, which will add 1 to each Int list item
  def addOne(l: List[Int]): List[Int] = {
    foldRight(l, Nil:List[Int])((x, xs) => Cons(x + 1, xs))
  }

  // Exercise 3.17: Implement `mapToString`
  def mapToString(l: List[Double]): List[String] = {
    foldRight(l, Nil:List[String])((x, xs) => Cons(x.toString, xs))
  }

  // Exercise 3.18: Implement `map`
  def map[A, B](l: List[A])(f: A => B): List[B] = {
    foldRight(l, Nil:List[B])((x, xs) => Cons(f(x), xs))
  }

  // Exercise 3.19: Implement `filter`
  def filter[A](l: List[A])(f: A => Boolean): List[A] = {
    foldRight(l, Nil:List[A])((x, xs) => if (f(x)) Cons(x, xs) else xs)
  }

  // Exercise 3.20: Implement `flatMap`
  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = {
    foldRight(l, Nil:List[B])((x, xs) => appendList(f(x), xs))
  }

  // Exercise 3.21: Implement `filterViaFlatMap`
  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] = {
    flatMap(l)(x => if (f(x)) List(x) else Nil)
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

  def testFoldLeft() = {
    println("--- testFoldLeft ---")
    val list3 = List(1, 2, 3)
    val list3D = List(1.0, 2.0, 3.0)
    val list1 = List(1)
    val list1D = List(1.0)

    printAndCheck(sum2(list3), sumFL(list3))
    printAndCheck(sum2(list1), sumFL(list1))
    printAndCheck(sum2(Nil),   sumFL(Nil))

    printAndCheck(product2(list3D), productFL(list3D))
    printAndCheck(product2(list1D), productFL(list1D))
    printAndCheck(product2(Nil),    productFL(Nil))

    printAndCheck(length(list3), lengthFL(list3))
    printAndCheck(length(list1), lengthFL(list1))
    printAndCheck(length(Nil),   lengthFL(Nil))

    println("--------------------\n")
  }

  def testReverse() = {
    println("--- testReverse ---")
    printAndCheck(List(3, 2, 1), reverse(List(1, 2, 3)))
    printAndCheck(List(1),       reverse(List(1)))
    printAndCheck(Nil,           reverse(Nil))
    println("-------------------\n")
  }

  def testAppend() = {
    println("--- testAppend ---")
    printAndCheck(List(1, 2, 3), append(List(1, 2), 3))
    printAndCheck(List(1, 2),    append(List(1), 2))
    printAndCheck(List(1),       append(Nil, 1))
    println("------------------\n")
  }

  def testConcatLists() = {
    println("--- testAppend ---")
    printAndCheck(List(1, 2, 3, 4, 5, 6), concatLists(List(List(1, 2), List(3, 4), List(5, 6))))
    printAndCheck(List(1, 2), concatLists(List(List(1), List(2))))
    printAndCheck(Nil,        concatLists(List(Nil)))
    printAndCheck(Nil,        concatLists(Nil))
    println("------------------\n")
  }

  def testAddOne() = {
    println("--- testAddOne ---")
    printAndCheck(List(1, 2, 3), addOne(List(0, 1, 2)))
    printAndCheck(List(1), addOne(List(0)))
    printAndCheck(Nil, addOne(Nil))
    println("------------------\n")
  }

  def testMapToString() = {
    println("--- testMapToString ---")
    printAndCheck(List("1.0", "2.0", "2.5"), mapToString(List(1.0, 2.0, 2.5)))
    printAndCheck(List("3.1415"), mapToString(List(3.1415)))
    printAndCheck(Nil, mapToString(Nil))
    println("-----------------------\n")
  }

  def testMap() = {
    println("--- testMap ---")
    printAndCheck(List("A", "B", "C"), map(List("a", "b", "c"))(_.toUpperCase))
    printAndCheck(List("A"), map(List("a"))(_.toUpperCase))
    printAndCheck(Nil, map(Nil:List[String])(_.toUpperCase))
    println("---------------\n")
  }

  def testFilter() = {
    println("--- testFilter ---")
    val f = (x: Int) => x % 2 != 0
    printAndCheck(List(1, 3), filter(List(1, 2, 3, 4))(f))
    printAndCheck(Nil, filter(List(2, 4))(f))
    printAndCheck(Nil, filter(Nil)(f))
    println("------------------\n")
  }

  def testFlatMap() = {
    println("--- testFlatMap ---")
    val f = (x: Int) => List(x, x)
    printAndCheck(List(1, 1, 2, 2, 3, 3), flatMap(List(1, 2, 3))(f))
    printAndCheck(List(1, 1), flatMap(List(1))(f))
    printAndCheck(Nil, flatMap(Nil)(f))
    println("-------------------\n")
  }

  def testFilterViaFlatMap() = {
    println("--- testFilterViaFlatMap ---")
    val f = (x: Int) => x % 2 != 0
    printAndCheck(List(1, 3), filterViaFlatMap(List(1, 2, 3, 4))(f))
    printAndCheck(Nil, filterViaFlatMap(List(2, 4))(f))
    printAndCheck(Nil, filterViaFlatMap(Nil)(f))
    println("----------------------------\n")
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
    testFoldLeft()
    testReverse()
    testAppend()
    testConcatLists()
    testAddOne()
    testMapToString()
    testMap()
    testFilter()
    testFlatMap()
    testFilterViaFlatMap()
  }
}
