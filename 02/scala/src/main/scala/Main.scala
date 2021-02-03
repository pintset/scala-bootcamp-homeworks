object Main extends App {
  sealed trait Shape extends Located with Bounded with Area

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

  sealed trait Volume {
    def volume: Double
  }

  final case class Point(x: Double, y: Double) extends Shape {
    override def minX: Double = x
    override def maxX: Double = x
    override def minY: Double = y
    override def maxY: Double = y

    override def area: Double = 1.0
  }

  final case class Circle(centerX: Double, centerY: Double, radius: Double) extends Shape {
    override def x: Double = centerX
    override def y: Double = centerY

    override def minX: Double = centerX - radius
    override def maxX: Double = centerX + radius
    override def minY: Double = centerY - radius
    override def maxY: Double = centerY + radius

    override def area: Double = Math.PI * radius * radius
  }

  case class Rectangle(centerX: Double, centerY: Double, width: Double, height: Double) extends Shape {
    private def halfOf(x: Double): Double = x / 2

    override def x: Double = centerX
    override def y: Double = centerY
    override def minX: Double = centerX - halfOf(width)
    override def maxX: Double = centerX + halfOf(width)
    override def minY: Double = centerY - halfOf(height)
    override def maxY: Double = centerY + halfOf(height)

    override def area: Double = width * height
  }

  final case class Square(centerX: Double, centerY: Double, side: Double) extends Shape {
    private val square = Rectangle(centerX, centerY, side, side)

    override def x: Double = centerX
    override def y: Double = centerY
    override def minX: Double = square.minX
    override def maxX: Double = square.maxX
    override def minY: Double = square.minY
    override def maxY: Double = square.maxY

    override def area: Double = square.area
  }

  final case class Triangle(xA: Double, yA: Double, xB: Double, yB: Double, xC: Double, yC: Double) extends Shape {
    private val xs = Array(xA, xB, xC)
    private val ys = Array(yA, yB, yC)
    private def avg(xs: Array[Double]): Double = xs.sum / xs.length

    // Centroid
    override def x: Double = avg(xs)
    override def y: Double = avg(ys)

    override def minX: Double = xs.min
    override def maxX: Double = xs.max
    override def minY: Double = ys.min
    override def maxY: Double = ys.max

    override def area: Double = Math.abs((xB - xA) * (yC - yA) - (xC - xA) * (yB - yA)) / 2
  }

  sealed trait Shape3d extends Shape with BoundedZ with LocatedZ with Volume

  sealed trait LocatedZ {
    def z: Double
  }

  sealed trait BoundedZ {
    def minZ: Double
    def maxZ: Double
  }

  final case class Point3d(x: Double, y: Double, z: Double) extends Shape3d {
    override def minX: Double = x
    override def maxX: Double = x
    override def minY: Double = y
    override def maxY: Double = y
    override def minZ: Double = z
    override def maxZ: Double = z

    override def area: Double = 1.0
    override def volume: Double = 1.0
  }

  final case class Sphere(centerX: Double, centerY: Double, centerZ: Double, radius: Double) extends Shape3d {
    override def x: Double = centerX
    override def y: Double = centerY
    override def z: Double = centerZ

    override def minX: Double = centerX - radius
    override def maxX: Double = centerX + radius
    override def minY: Double = centerY - radius
    override def maxY: Double = centerY + radius
    override def minZ: Double = centerZ - radius
    override def maxZ: Double = centerZ + radius

    override def area: Double = 4 * Math.PI * radius * radius
    override def volume: Double = 4 * Math.PI * Math.pow(radius, 3) / 3
  }

  case class Cuboid(centerX: Double, centerY: Double, centerZ: Double, width: Double, height: Double, depth: Double) extends Shape3d {
    private def halfOf(x: Double): Double = x / 2

    override def x: Double = centerX
    override def y: Double = centerY
    override def z: Double = centerZ
    override def minX: Double = centerX - halfOf(width)
    override def maxX: Double = centerX + halfOf(width)
    override def minY: Double = centerY - halfOf(height)
    override def maxY: Double = centerY + halfOf(height)
    override def minZ: Double = centerZ - halfOf(depth)
    override def maxZ: Double = centerZ + halfOf(depth)

    override def area: Double = 2 * (width * height + height * depth + width * depth)
    override def volume: Double = width * height * depth
  }

  final case class Cube(val centerX: Double, val centerY: Double, val centerZ: Double, side: Double) extends Shape3d {
    private val cube = Cuboid(centerX, centerY, centerZ, side, side, side)

    override def x: Double = centerX
    override def y: Double = centerY
    override def z: Double = centerZ
    override def minX: Double = cube.minX
    override def maxX: Double = cube.maxX
    override def minY: Double = cube.minY
    override def maxY: Double = cube.maxY
    override def minZ: Double = cube.minZ
    override def maxZ: Double = cube.maxZ

    override def area: Double = cube.area
    override def volume: Double = cube.volume
  }

  final case class Triangle3d(xA: Double, yA: Double, zA: Double, xB: Double, yB: Double, zB: Double, xC: Double, yC: Double, zC: Double, origin: Point3d) extends Shape3d {
    private val xs = Array(xA, xB, xC)
    private val ys = Array(yA, yB, yC)
    private val zs = Array(zA, zB, zC)

    private def avg(xs: Array[Double]): Double = xs.sum / xs.length

    // Centroid
    override def x: Double = avg(xs)
    override def y: Double = avg(ys)
    override def z: Double = avg(zs)

    override def minX: Double = xs.min
    override def maxX: Double = xs.max
    override def minY: Double = ys.min
    override def maxY: Double = ys.max
    override def minZ: Double = zs.min
    override def maxZ: Double = zs.max

    override def area: Double = {
      def sqr(x: Double): Double = x * x

      Math.sqrt(
        sqr(xB * yA - xC * yA - xA * yB + xC * yB + xA * yC - xB * yC)
          + sqr(xB * zA - xC * zA - xA * zB + xC * zB + xA * zC - xB * zC)
          + sqr(yB * zA - yC * zA - yA * zB + yC * zB + yA * zC - yB * zC)) / 2
    }

    override def volume: Double = ???
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