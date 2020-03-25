object HandleException {
  def unSafe[T](ex: Exception)(block: => T) = try {
    block
  } catch {
    case e: Throwable => {
      println(s"Exception occured: ${e.getMessage()}")
      throw ex;
    }
  }
}

object Utils {
  def isSorted(as: List[Int], ordering: (Int, Int) => Boolean): Boolean = as match {
    case Nil | _ :: Nil => true
    case x :: xs => ordering(x, xs.head) && isSorted(xs, ordering)
  }
  
  def isAscSorted(as: List[Int]) = isSorted(as, (a: Int, b: Int) => a <= b)

  def isDescSorted(as: List[Int]) = isSorted(as, (a: Int, b: Int) => a >= b)
  
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    def helper(as: List[A], acc: B): B = as match {
      case Nil => acc
      case x :: xs => helper(xs, f(acc, x))
    }

    helper(l, z)
  }

  def sum(l: List[Int]) = foldLeft(l, 0)((a, b) => a + b)
  
  def length[A](l: List[A]) = foldLeft(l, 0)((a, b) => a + 1)
  
  def compose[A, B, C](f: B => C, g: A => B) = (x: A) => f(g(x))

  // if we want to compose f with f a type of the function has to be A => A not A => B, so I changed a bit signature of the repeated function from the task
  def repeated[A](f: A => A, n: Int) = {
    require(n > 0)
    foldLeft(List.fill(n - 1)(f), f)((f: A => A, g: A => A) => compose(f, g))
  }
  
  def curry[A, B, C](f: (A, B) => C) = (a: A) => (b: B) => f(a, b) 

  def uncurry[A, B, C](f: A => B => C) = (a: A, b: B) => f(a)(b)
}

object Main extends App {
  val sortedAscending = List(1, 2, 3, 4, 10, 11, 21, 32, 33, 1000)
  val sortedAscending2 = List(1, 1, 3, 4, 10, 10, 10, 32, 33, 33)
  val sortedDescending = List(10, 8, 7, 7, 7, 6, 3, 0, -500)
  require(Utils.isSorted(sortedAscending, (a: Int, b: Int) => a < b))
  require(!Utils.isSorted(sortedAscending2, (a: Int, b: Int) => a < b))
  require(Utils.isAscSorted(sortedAscending2))  
  require(Utils.isDescSorted(sortedDescending))

  require(Utils.sum(sortedAscending) == 1117)
  require(Utils.length(sortedAscending) == 10)

  require(Utils.compose((x: Int) => x + 1, (x: Int) => x * x)(2) == 5)
  require(Utils.repeated((x: Int) => x * x, 3)(3) == 6561)

  val add = (a: Int, b: Int) => a + b
  val addCurried = Utils.curry(add)
  require(addCurried(1)(1) == 2)
  require(Utils.uncurry(addCurried)(1, 1) == 2)

  //HandleException.unSafe(new Exception("Testing exception")) {
  //  println(10 / 0)
  //  println(Utils.repeated((x: Int) => x * x, 0)(3))
  //}
}