package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  // Exercise 3.25: Implement `size` that counts the number of nodes (leaves and branches) in a tree
  def size[A](t: Tree[A]): Int = t match {
    case Branch(left, right) => size(left) + size(right) + 1
    case Leaf(_) => 1
    case null => 0
  }

  // Exercise 3.26: Implement `maximum` that returns the maximum element in a Tree[Int]
  def maximum(t: Tree[Int]): Int = t match {
    case Branch(left, right) => maximum(left) max maximum(right)
    case Leaf(v) => v
    case null => Int.MinValue
  }

  // Exercise 3.27: Implement `depth` that returns the maximum path length from the root of a tree to any leaf
  def depth[A](t: Tree[A]): Int = t match {
    case Branch(left, right) => (depth(left) max depth(right)) + 1
    case Leaf(_) => 1
    case null => 0
  }

  // Exercise 3.28: Implement `map` that modifies each element in a tree with a given function
  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    case Leaf(v) => Leaf(f(v))
    case null => null
  }
}

object TreeTest {

  import Tree._
  import fpinscala.util.TestUtils._

  val fullTree = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
  val leftTree = Branch(Branch(Leaf(1), Leaf(2)), null)
  val rightTree = Branch(null, Branch(Leaf(3), Leaf(4)))
  val seldomLeavesTree = Branch(Branch(Leaf(1), null), Branch(null, Leaf(4)))

  val fullTreeStr = Branch(Branch(Leaf("1_1"), Leaf("2_2")), Branch(Leaf("3_3"), Leaf("4_4")))
  val leftTreeStr = Branch(Branch(Leaf("1_1"), Leaf("2_2")), null)
  val rightTreeStr = Branch(null, Branch(Leaf("3_3"), Leaf("4_4")))
  val seldomLeavesTreeStr = Branch(Branch(Leaf("1_1"), null), Branch(null, Leaf("4_4")))

  def testSize() = {
    println("--- testSize ---")
    printAndCheck(7, size(fullTree))
    printAndCheck(0, size(null))
    printAndCheck(4, size(leftTree))
    printAndCheck(4, size(rightTree))
    printAndCheck(5, size(seldomLeavesTree))
    println("----------------\n")
  }

  def testMaximum() = {
    println("--- testMaximum ---")
    printAndCheck(4,            maximum(fullTree))
    printAndCheck(Int.MinValue, maximum(null))
    printAndCheck(2,            maximum(leftTree))
    printAndCheck(4,            maximum(rightTree))
    printAndCheck(4,            maximum(seldomLeavesTree))
    println("-------------------\n")
  }

  def testDepth() = {
    println("--- testDepth ---")
    printAndCheck(3, depth(fullTree))
    printAndCheck(0, depth(null))
    printAndCheck(3, depth(leftTree))
    printAndCheck(3, depth(rightTree))
    printAndCheck(3, depth(seldomLeavesTree))
    println("-----------------\n")
  }

  def testMap() = {
    println("--- testMap ---")
    val f = (v: Int) => v + "_" + v
    printAndCheck(fullTreeStr,         map(fullTree)(f))
    printAndCheck(null,                map(null)(f))
    printAndCheck(leftTreeStr,         map(leftTree)(f))
    printAndCheck(rightTreeStr,        map(rightTree)(f))
    printAndCheck(seldomLeavesTreeStr, map(seldomLeavesTree)(f))
    println("---------------\n")
  }

  def main(args: Array[String]) = {
    testSize()
    testMaximum()
    testDepth()
    testMap()
  }
}
