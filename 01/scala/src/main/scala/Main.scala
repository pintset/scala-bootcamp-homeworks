object Main extends App {
  // Classic
  def gcd(a: Int, b: Int): Int = {
    lazy val inner: (Int, Int) => Int = (a: Int, b: Int) => {
      val m = a % b
      if (m == 0) b else inner(b, m)
    }
    if (a > b) inner(a, b) else inner(b, a)
  }

  def lcm(a: Int, b: Int): Int = a * b / gcd(a, b)

  // Opt
  def toTupleOpt[A, B](a: Option[A], b: Option[B]) = (a, b) match {
    case (Some(x), Some(y)) => Some((x, y))
    case _ => None
  }

  val toNatural = (x: Int) => if (x > 0) Some(x) else None
  val toNaturalTuple = (a: Int, b: Int) => toTupleOpt(toNatural(a), toNatural(b))

  val gcdOpt = (a: Int, b: Int) => {
    lazy val inner: (Int, Int) => Int = (a: Int, b: Int) => {
      val m = a % b
      if (m == 0) b else inner(b, m)
    }

    toNaturalTuple(a, b).map(x => {
      val (a, b) = x
      if (a > b) inner(a, b) else inner(b, a)
    })
  }

  val lcmOpt = (a: Int, b: Int) => gcdOpt(a, b).map(gcd => a * b / gcd)

  // Execute
  def run[R](fGcd: (Int, Int) => R, fLcm: (Int, Int) => R): Unit = {
    val input = (message: String) => {
      print(message)
      try {
        Some(scala.io.StdIn.readInt())
      } catch {
        case _: Exception => None
      }
    }

    val a = input("Enter a: ")
    val b = input("Enter b: ")

    val result =
      toTupleOpt(a, b).map(x => {
        val (a, b) = x
        s"gcd($a, $b) = ${fGcd(a, b)} and lcm($a, $b) = ${fLcm(a, b)}"
      }).getOrElse("Unable to parse parameters")
    println(result)
  }

  // run(gcd, lcm)
  run(gcdOpt, lcmOpt)
}