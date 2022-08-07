object Main extends App {
  sealed trait Located {
    def x: Double
    def y: Double
  }

  sealed trait Bounded {
    def minX: Double
    def maxX: Double
    def minY: Double
    def maxY: Double
  }

  sealed trait Area {
    def area: Double
  }

  sealed trait Shape extends Located with Bounded with Area

  final case class Point(x: Double, y: Double) extends Shape {
    def minX: Double = x
    def maxX: Double = x
    def minY: Double = y
    def maxY: Double = y

    def area: Double = 1.0
  }

  final case class Circle(centerX: Double, centerY: Double, radius: Double) extends Shape {
    def x: Double = centerX
    def y: Double = centerY

    def minX: Double = centerX - radius
    def maxX: Double = centerX + radius
    def minY: Double = centerY - radius
    def maxY: Double = centerY + radius

    def area: Double = Math.PI * radius * radius
  }

  final case class Rectangle(centerX: Double, centerY: Double, width: Double, height: Double) extends Shape {
    private def halfOf(x: Double): Double = x / 2

    def x: Double = centerX
    def y: Double = centerY
    def minX: Double = centerX - halfOf(width)
    def maxX: Double = centerX + halfOf(width)
    def minY: Double = centerY - halfOf(height)
    def maxY: Double = centerY + halfOf(height)

    def area: Double = width * height
  }

  final case class Square(centerX: Double, centerY: Double, side: Double) extends Shape {
    private val square = Rectangle(centerX, centerY, side, side)

    def x: Double = centerX
    def y: Double = centerY
    def minX: Double = square.minX
    def maxX: Double = square.maxX
    def minY: Double = square.minY
    def maxY: Double = square.maxY

    def area: Double = square.area
  }

  final case class Triangle(xA: Double, yA: Double, xB: Double, yB: Double, xC: Double, yC: Double) extends Shape {
    private val xs = Array(xA, xB, xC)
    private val ys = Array(yA, yB, yC)
    private def avg(xs: Array[Double]): Double = xs.sum / xs.length

    // Centroid
    def x: Double = avg(xs)
    def y: Double = avg(ys)

    def minX: Double = xs.min
    def maxX: Double = xs.max
    def minY: Double = ys.min
    def maxY: Double = ys.max

    def area: Double = Math.abs((xB - xA) * (yC - yA) - (xC - xA) * (yB - yA)) / 2
  }

  sealed trait LocatedZ {
    def z: Double
  }

  sealed trait BoundedZ {
    def minZ: Double
    def maxZ: Double
  }

  sealed trait Volume {
    def volume: Double
  }

  sealed trait Shape3d extends Shape with BoundedZ with LocatedZ with Volume

  final case class Point3d(x: Double, y: Double, z: Double) extends Shape3d {
    def minX: Double = x
    def maxX: Double = x
    def minY: Double = y
    def maxY: Double = y
    def minZ: Double = z
    def maxZ: Double = z

    def area: Double = 1.0
    def volume: Double = 1.0
  }

  final case class Sphere(centerX: Double, centerY: Double, centerZ: Double, radius: Double) extends Shape3d {
    def x: Double = centerX
    def y: Double = centerY
    def z: Double = centerZ

    def minX: Double = centerX - radius
    def maxX: Double = centerX + radius
    def minY: Double = centerY - radius
    def maxY: Double = centerY + radius
    def minZ: Double = centerZ - radius
    def maxZ: Double = centerZ + radius

    def area: Double = 4 * Math.PI * radius * radius
    def volume: Double = 4 * Math.PI * Math.pow(radius, 3) / 3
  }

  final case class Cuboid(centerX: Double, centerY: Double, centerZ: Double, width: Double, height: Double, depth: Double) extends Shape3d {
    private def halfOf(x: Double): Double = x / 2

    def x: Double = centerX
    def y: Double = centerY
    def z: Double = centerZ
    def minX: Double = centerX - halfOf(width)
    def maxX: Double = centerX + halfOf(width)
    def minY: Double = centerY - halfOf(height)
    def maxY: Double = centerY + halfOf(height)
    def minZ: Double = centerZ - halfOf(depth)
    def maxZ: Double = centerZ + halfOf(depth)

    def area: Double = 2 * (width * height + height * depth + width * depth)
    def volume: Double = width * height * depth
  }

  final case class Cube(centerX: Double, centerY: Double, centerZ: Double, side: Double) extends Shape3d {
    private val cube = Cuboid(centerX, centerY, centerZ, side, side, side)

    def x: Double = centerX
    def y: Double = centerY
    def z: Double = centerZ
    def minX: Double = cube.minX
    def maxX: Double = cube.maxX
    def minY: Double = cube.minY
    def maxY: Double = cube.maxY
    def minZ: Double = cube.minZ
    def maxZ: Double = cube.maxZ

    def area: Double = cube.area
    def volume: Double = cube.volume
  }

  final case class Triangle3d(xA: Double, yA: Double, zA: Double, xB: Double, yB: Double, zB: Double, xC: Double, yC: Double, zC: Double, origin: Point3d) extends Shape3d {
    private val xs = Array(xA, xB, xC)
    private val ys = Array(yA, yB, yC)
    private val zs = Array(zA, zB, zC)

    private def avg(xs: Array[Double]): Double = xs.sum / xs.length

    // Centroid
    def x: Double = avg(xs)
    def y: Double = avg(ys)
    def z: Double = avg(zs)

    def minX: Double = xs.min
    def maxX: Double = xs.max
    def minY: Double = ys.min
    def maxY: Double = ys.max
    def minZ: Double = zs.min
    def maxZ: Double = zs.max

    def area: Double = {
      def sqr(x: Double): Double = x * x

      Math.sqrt(
        sqr(xB * yA - xC * yA - xA * yB + xC * yB + xA * yC - xB * yC)
          + sqr(xB * zA - xC * zA - xA * zB + xC * zB + xA * zC - xB * zC)
          + sqr(yB * zA - yC * zA - yA * zB + yC * zB + yA * zC - yB * zC)) / 2
    }

    def volume: Double = ???
  }

  def printArea(shape: Area): Unit = println(s"$shape area is ${shape.area}")
  def printVolume(shape: Volume): Unit = println(s"$shape volume is ${shape.volume}")

  Array(Circle(5, 6, 2), Rectangle(0, 0, 10, 5), Square(0, 0, 5), Triangle(-2, -2, 1, 2, 3, -1)).foreach(printArea(_))

  val shapes3d = Array(Sphere(5, 6, 0, 5), Cuboid(1, 1, 1, 3, 4, 5), Cube(1, 1, 1, 3), Triangle3d(-2, -2, -2, 1, 2, 4, 3, -1, -5, Point3d(0, 0, 0)))
  shapes3d.foreach(printArea(_))
  shapes3d.filter {
    case Triangle3d(_, _, _, _, _, _, _, _, _, _) => false
    case _ => true
  }.foreach(printVolume(_))
}