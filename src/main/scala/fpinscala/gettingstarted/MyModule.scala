package fpinscala.gettingstarted

object MyModule {

  // Exercise 1: Write a function to compute the n-th fibonacci number
  def fib(n: Int): Int = {
    if (n < 0)
      return -fib(-n)

    @annotation.tailrec
    def loop(n: Int, a: Int, b: Int): Int = {
      if (n == 0) a
      else loop(n - 1, b, a + b)
    }
    loop(n, 0, 1)
  }

  // Exercise 2: Implement a polymorphic function to check whether an `Array[A]` is sorted
  def isSorter[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    require(as.length > 1)

    @annotation.tailrec
    def loop(n: Int): Boolean = {
      val res = ordered(as(n), as(n + 1))
      if (n == 0) res
      else if (res) loop(n - 1)
      else false
    }
    loop(as.length - 2)
  }
}

object TestMyModule {

  import MyModule._

  def testFib() = {
    println("--- testFib ---")
    val answers = List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55)
    var i = 0
    for (i <- 0 to 10) {
      val res = fib(i)
      println("%2d: %2d == %2d".format(i, answers(i), res))
      require(answers(i) == res)
    }
    println("---------------\n")
  }

  def testIsSorter() = {
    println("--- testIsSorter ---")
    val arr = Array(
      (Array(1, 2, 3, 4, 5), true),
      (Array(1, 2, 7, 4, 5), false)
    )
    for ((as, require) <- arr) {
      val res = isSorter(as, (x: Int, y: Int) => x <= y)
      println("%5s == %5s: %s".format(res, require, as.mkString("[", ",", "]")))
    }
    println("--------------------\n")
  }

  def main(args: Array[String]): Unit = {
    testFib()
    testIsSorter()
  }
}
