package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  // Exercise 3.25: Implement `size` that counts the number of nodes (leaves and branches) in a tree
  def size[A](t: Tree[A]): Int = t match {
    case Branch(left, right) => size(left) + size(right) + 1
    case Leaf(_) => 1
    case _ => 0
  }
}

object TreeTest {

  import Tree._
  import fpinscala.util.TestUtils._

  val fullTree = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
  val leftTree = Branch(Branch(Leaf(1), Leaf(2)), null)
  val rightTree = Branch(null, Branch(Leaf(3), Leaf(4)))
  val seldomLeavesTree = Branch(Branch(Leaf(1), null), Branch(null, Leaf(4)))

  def testSize() = {
    println("--- testSize ---")
    printAndCheck(7, size(fullTree))
    printAndCheck(0, size(null))
    printAndCheck(4, size(leftTree))
    printAndCheck(4, size(rightTree))
    printAndCheck(5, size(seldomLeavesTree))
    println("----------------\n")
  }

  def main(args: Array[String]) = {
    testSize()
  }
}
