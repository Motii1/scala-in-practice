// Dawid Motak
// 309318

package numbers {
  class Rational(n: Int, d: Int = 1) {
    require(d != 0)
    if (n == 0 || n == 1)
      require(d == 1)

    private val g = Rational.gcd(n.abs, d.abs)

    // if number is negative, a minus should only be in numerator
    val num: Int = if (d > 0) n / g else -n / g
    val den: Int = if (d > 0) d / g else -d / g

    def +(other: Rational): Rational = {
      val newN = this.num * other.den + other.num * this.den
      val newD = this.den * other.den

      val divisor = Rational.gcd(newN.abs, newD)
      new Rational(newN / divisor, newD / divisor)
    }

    def unary_- = new Rational(-this.num, den)

    def -(other: Rational): Rational = this + (-other)
    
    def *(other: Rational): Rational = {
      val newN = this.num * other.num
      val newD = this.den * other.den

      val divisor = Rational.gcd(newN.abs, newD)
      new Rational(newN / divisor, newD / divisor)
    }
    def /(other: Rational): Rational = this * new Rational(other.den, other.num)
    
    def ==(other: Rational): Boolean = this.num == other.num && other.den == this.den

    override def toString: String = {
      val number = this.num / this.den;
      if (number == 0)
        this.num + " / " + this.den
      else if (this.num % this.den == 0)
        number.toString
      else
        number + " " + this.num % this.den + " / " + this.den
    }
  }

  object Rational {
    private def gcd(x: Int, y: Int): Int = if (y == 0) x else gcd(y, x % y)

    def apply(num: Int, den: Int = 1): Rational = new Rational(num, den)
    def zero = new Rational(0, 1)
    def one = new Rational(1, 1)
  }
}

package figures {
  import numbers._
  import math.sqrt
  import math.pow

  class Point(val x: Rational, val y: Rational) {
    def distance(other: Point): Double = {
      val xToDouble: Double = this.x.num.toDouble / this.x.den
      val yToDouble: Double = this.y.num.toDouble / this.y.den

      val xOtherToDouble: Double = other.x.num.toDouble / other.x.den
      val yOtherToDouble: Double = other.y.num.toDouble / other.y.den
 
      val resX = pow(xToDouble - xOtherToDouble, 2)
      val resY = pow(yToDouble - yOtherToDouble, 2)
      sqrt(resX + resY)
    }

    // tests if this, other1, other2 is a right angle
    def isOrthogonal(other1: Point, other2: Point): Boolean = {
      (other1.x - this.x) * (other1.x - other2.x) + (other1.y - this.y) * (other1.y - other2.y) == Rational.zero
    }
  }

  trait Shape {
    def area: Double
    val description: String
  }

  class Triangle(val v1: Point, val v2: Point, val v3: Point) extends Shape {
    val a = v1.distance(v2)
    val b = v1.distance(v3)
    val c = v2.distance(v3)

    require(a + b > c && a + c > b && b + c > a)
    
    def area: Double = {
      val s = (this.a + this.b + this.c) / 2
      sqrt(s * (s - this.a) * (s - this.b) * (s - this.c))
    }

    val description: String = "Triangle"
  }

  object Triangle {
    def apply(v1: Point, v2: Point, v3: Point): Triangle = new Triangle(v1, v2, v3)

    def rightTriangle(v1: Point, height: Int, width: Int) = {
      val v2 = new Point(v1.x, v1.y + Rational(height))
      val v3 = new Point(v1.x + Rational(width), v1.y)
      new Triangle(v1, v2, v3)
    }
  }

  class Rectangle(val v1: Point, val v2: Point, val v3: Point, val v4: Point) extends Shape {
    private def isRectangle(v1: Point, v2: Point, v3: Point, v4: Point): Boolean = {
      v1.isOrthogonal(v2, v3) &&
      v2.isOrthogonal(v3, v4) &&
      v3.isOrthogonal(v4, v1)
    }

    protected def isRectangleAnyOrder: Boolean = {
      isRectangle(v1, v2, v3, v4) ||
      isRectangle(v2, v3, v1, v4) ||
      isRectangle(v3, v1, v2, v4) 
    }

    require(isRectangleAnyOrder)

    def area: Double = {
      val s1 = v1.distance(v2)
      val s2 = v1.distance(v3)
      val s3 = v1.distance(v4)
      if (s3 > s2 && s3 > s1)
        s2 * s1
      else if (s2 > s3 && s2 > s1)
        s3 * s1
      else
        s2 * s3
    }
    val description: String = "Rectangle"
  }

  object Rectangle {
    def apply(v1: Point, v2: Point, v3: Point, v4: Point) = new Rectangle(v1, v2, v3, v4)
  }

  class Square(v1: Point, v2: Point, v3: Point, v4: Point) extends Rectangle(v1, v2, v3, v4) {
    private def isSquare: Boolean = {
      if (!isRectangleAnyOrder)
        false
      else {
        val line1 = v1.distance(v2)
        val line2 = v1.distance(v3)
        val line3 = v1.distance(v4)

        line1 == line2 || line1 == line3 || line2 == line3
      }
    }
    require(isSquare)

    override val description: String = "Square"
  }

  object Square {
    def apply(v1: Point, v2: Point, v3: Point, v4: Point) = new Square(v1, v2, v3, v4)
  }
}

object Singleton {
  import figures._
  def areaSum(figures: List[Shape]): Double = figures.foldLeft(0.0)((a, b) => a + b.area)
  def printAll(figures: List[Shape]): Unit = figures.foreach(arg => println(arg.description))
}

object Main extends App {
  import figures._
  import numbers._

  val rat1 = new Rational(2, 7)
  val rat2 = new Rational(3, 4)
  val rat3 = new Rational(50, 6)

  val addition = rat1 + rat2
  val subtraction = rat1 - rat2
  val multiplication = rat1 * rat2
  val division = rat1 / rat2

  require(multiplication == Rational(3, 14))
  require(addition == Rational(29, 28))
  require(subtraction == Rational(-13, 28))
  require(division == Rational(8, 21))
  require(rat1 == Rational.one / (Rational.one / rat1))
  require(rat3.toString == "8 1 / 3")

  val p1 = new Point(Rational.one, Rational.one)
  val p2 = new Point(Rational(6), Rational.one)
  val p3 = new Point(Rational(6), Rational(3))
  val p4 = new Point(Rational.one, Rational(3))
  val p5 = new Point(Rational(3), Rational(1))
  val p6 = new Point(Rational(3), Rational(3))

  val triangle = new Triangle(p1, p2, p4)
  val square = new Square(p1, p4, p5, p6)
  val rectangle = new Rectangle(p1, p2, p3, p4)

  val shapes: List[Shape] = List(triangle, square, rectangle)
  println(Singleton.areaSum(shapes))
  require(Singleton.areaSum(shapes).floor == 19.0)
  Singleton.printAll(shapes)
}