import scala.annotation.tailrec
import scala.util.Try

object Main extends App {
  // https://en.wikipedia.org/wiki/Greatest_common_divisor
  def gcd(a: Int, b: Int): Int = {
    @tailrec
    def inner(a: Int, b: Int): Int = {
      val m = a % b
      if (m == 0) b else inner(b, m)
    }

    if (a > b) inner(a, b) else inner(b, a)
  }

  // https://en.wikipedia.org/wiki/Lowest_common_denominator
  def lcm(a: Int, b: Int): Int = a * b / gcd(a, b)

  def getParameter(name: String): Option[Int] = {
    print(s"Enter $name: ")
    Try(scala.io.StdIn.readInt).toOption.flatMap { x => if (x > 0) Some(x) else None }
  }

  val result =
    getParameter("a").flatMap { a =>
      getParameter("b").map { b =>
        s"gcd($a, $b) = ${gcd(a, b)} and lcm($a, $b) = ${lcm(a, b)}"
      }
    }.getOrElse("Failed to parse parameter. Natural number expected")

  println(result)
}