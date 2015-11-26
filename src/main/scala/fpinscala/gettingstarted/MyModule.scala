package fpinscala.gettingstarted

object MyModule {

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
    println("---------------")
  }

  def main(args: Array[String]): Unit = {
    testFib()
  }
}
