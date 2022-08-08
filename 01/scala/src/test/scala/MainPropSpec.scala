import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Gen

object Gens {
  val naturalInts: Gen[Int] = Gen.choose(1, 5000)
}

import Gens._
import Main._

object GcmSpec extends Properties("gcm") {
  property("f(a * c, b * c) = c * f(a, b)") = forAll(naturalInts, naturalInts, naturalInts) { (a: Int, b: Int, c: Int) =>
    gcd(a * c, b * c) == c * gcd(a, b)
  }

  property("f(n, n + k) = f(n, k)") = forAll(naturalInts, naturalInts) { (n: Int, k: Int) =>
    gcd(n, n + k) == gcd(n, k)
  }

  property("f(a, b) = f(b, a)") = forAll(naturalInts, naturalInts) { (a: Int, b: Int) =>
    gcd(a, b) == gcd(b, a)
  }
}

object LcdSpec extends Properties("lcd") {
  property("f(a, b) = f(b, a)") = forAll(naturalInts, naturalInts) { (a: Int, b: Int) =>
    lcm(a, b) == lcm(b, a)
  }
}
