import scala.annotation.nowarn
import org.scalatest.EitherValues
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import Main._

@nowarn
class MainSpec extends AnyFreeSpec with Matchers with EitherValues {

  "gcd of" - {
    "24 and 18 is 6" in {
      gcd(24, 18) shouldEqual 6
    }

    "12 and 9 is 3" in {
      gcd(12, 9) shouldEqual 3
    }

    "28 and 16 is 4" in {
      gcd(28, 16) shouldEqual 4
    }
  }

  "lcm of" - {
    "9 and 12 is 36" in {
      lcm(9, 12) shouldEqual 36
    }

    "24 and 12 is 24" in {
      lcm(24, 12) shouldEqual 24
    }

    "98 and 35 is 490" in {
      lcm(98, 35) shouldEqual 490
    }
  }
}