package fpinscala.datastructures

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

  def testPatternMatching() = {
    println("--- testEx1 ---")
    printAndCheck(1, patternMatching(List(1, 2, 4, 5)))
    printAndCheck(42, patternMatching(Nil))
    printAndCheck(42, patternMatching(List()))
    printAndCheck(3, patternMatching(List(1, 2, 3, 4, 5)))
    printAndCheck(12, patternMatching(List(3, 4, 5)))
    printAndCheck(101, patternMatching(null))
    println("---------------\n")
  }

  private def printAndCheck[A](expected: A, actual: A) = {
    println("%s == %s".format(expected, actual))
    require(expected == actual)
  }

  def main(args: Array[String]) = {
    testPatternMatching()
  }
}
