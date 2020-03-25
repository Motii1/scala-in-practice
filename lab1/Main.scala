object Main extends App {
  //scalar product of two vectors xs and ys
  def scalarUgly(xs: List[Int], ys: List[Int]) = {
    require(xs.size == ys.size)
    var i = 0
    var product = 0
    while (i < xs.size) {
      product += xs(i) * ys(i)
      i += 1
    }
    product
  }

  def scalar(xs: List[Int], ys: List[Int]) = {
    require(xs.size == ys.size)
    (for ((x, y) <- (xs zip ys)) yield x * y).sum 
  }

  //quicksort algorithm
  def sortUgly(xs: List[Int]): List[Int] = {
    if (xs == Nil) {
      xs
    } else {
      var i = 1
      var pivot = xs(0)
      var listSmallerOrEqual = List[Int]()
      var listBigger = List[Int]()
      while (i < xs.size) {
        if (xs(i) < pivot) {
          listSmallerOrEqual = xs(i) +: listSmallerOrEqual
        } else {
          listBigger = listBigger :+ xs(i)
        }
        i += 1
      }

      sortUgly(listSmallerOrEqual) ::: (pivot +: sortUgly(listBigger))
    }
  }
  def sort(xs: List[Int]): List[Int] = {
    if (xs == Nil) {
      xs
    } else {
      val pivot = xs.head
      val listSmallerOrEqual = for (x <- xs.tail if x <= pivot) yield x
      val listBigger = for (x <- xs.tail if x > pivot) yield x
      sort(listSmallerOrEqual) ::: (pivot +: sort(listBigger))
    }
  }
  
  //checks if n is prime
  def isPrimeUgly(n: Int): Boolean = {
    if (n < 2)
      return false
    var i = 2
    while (i <= n / 2) {
      if (n % i == 0)
        return false
      i += 1
    }
    true
  }
  def isPrime(n: Int): Boolean = {
    n >= 2 && (for (i <- 2 to n / 2 if n % i == 0) yield i) == Nil
  }
  
  //for given positive integer n, find all pairs of integers i and j, where 1 ≤ j < i < n such that i + j is prime
  def primePairsUgly(n : Int): List[(Int, Int)] = {
    var i = 1
    var resultList = List[(Int, Int)]()
    while (i < n) {
      var j = i + 1
      while (j < n) {
        if (isPrime(i + j)) {
          resultList = (i, j) +: resultList
        }
        j += 1
      }
      i += 1
    }
    resultList
  }
  def primePairs(n : Int): List[(Int, Int)] = {
    for {
      i <- (1 to n - 1).toList;
      j <- i + 1 to n - 1;
      if isPrime(i + j)
    } yield (i, j)
  }
  
  //create a list with all lines from given file
  val filesHere = new java.io.File(".").listFiles
  def fileLinesUgly(file: java.io.File): List[String] = {
    var linesIt = scala.io.Source.fromFile(file, "ISO-8859-1").getLines
    var resultList = List[String]()
    while (linesIt.hasNext) {
      resultList = resultList :+ linesIt.next
    }
    resultList
  }
  def fileLines(file: java.io.File): List[String] = {
    scala.io.Source.fromFile(file, "ISO-8859-1").getLines.toList
  }
  
  //print names of all .scala files which are in filesHere & are non empty
  def printNonEmptyUgly(pattern: String): Unit = {
    require(new java.io.File(pattern).isDirectory)
    var filesHere = new java.io.File(pattern).listFiles
    var len = filesHere.length
    var i = 0
    while (i < len) {
      var file = filesHere(i)
      if (file.length != 0 && file.toString.split("\\.").last == "scala") {
        println(file.toString)
      }
      i += 1
    }
  }
  def printNonEmpty(pattern: String): Unit = {
    require(new java.io.File(pattern).isDirectory)
    val filesHere = new java.io.File(pattern).listFiles
    filesHere.filter(f => f.length != 0 && f.toString.split("\\.").last == "scala").foreach(f => println(f => f.toString))
  }
}
