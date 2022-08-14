package testing

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalacheck.Test.Parameters

class JsonSpec extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks with Matchers {
  import Json._

  implicit val params = Parameters.default.withMinSuccessfulTests(1000)

  def jsonGen: Gen[String] = {
    val doubleGen = Arbitrary.arbitrary[Double].filter(d => d.isFinite).map(_.toString)
    val intGen = Arbitrary.arbitrary[Int].map(_.toString)
    val numGen = Gen.oneOf(doubleGen, intGen)

    def wrap(s: String): String = '"' + s + '"'

    val stringGen = Gen.sized(size => Gen.listOfN(size + 1, Gen.alphaChar).map(_.mkString)).map(wrap)

    val nullGen = Gen.const("null")

    val boolGen = Gen.oneOf(Gen.const("true"), Gen.const("false"))

    val primitiveGen = Gen.oneOf(numGen, stringGen, nullGen, boolGen)

    def valueGen(s: Int): Gen[String] = s match {
      case 0 => primitiveGen
      case n if n > 0 =>
        Gen.oneOf(primitiveGen, objectGen(n/2), arrayGen(n/2))
      case _ => fail("Only positive arguments are allowed")
    }

    def objectGen(s: Int): Gen[String] = s match {
      case 0 => Gen.const("{}")
      case n if n > 0 =>
        val fieldGen = for {
          name <- stringGen
          value <- valueGen(n/2)
        } yield (name, value)

        Gen.listOf(fieldGen).map { x =>
          val s = x.distinctBy(_._1).map { case (n, v) => s"$n:$v" }.mkString(",")
          s"{$s}"
        }
      case _ => fail("Only positive arguments are allowed")
    }

    def arrayGen(s: Int): Gen[String] = s match {
      case 0 => Gen.const("[]")
      case n if n > 0 =>
        Gen.listOf(valueGen(n / 2)).map { x =>
          val s = x.mkString(",")
          s"[$s]"
        }
      case _ => fail("Only positive arguments are allowed")
    }

    Gen.sized(valueGen)
  }

  "parse with subsequent print" should "result in the original json" in {
    forAll(jsonGen) { json =>
      Json.parse(json).map(Json.print).contains(json)
    }
  }
}
