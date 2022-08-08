package testing

import org.scalatest.EitherValues
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import scala.annotation.nowarn

@nowarn
class CalculatorSpec extends AnyFreeSpec with Matchers with EitherValues {

  "calculator" - {
    "enters the number correctly" in {
      val calculator = Calculator()

      calculator.enter(1).right.value shouldBe Calculator(0, 1, None)
      calculator.enter(7).right.value shouldBe Calculator(0, 7, None)
      calculator.enter(12).left.value shouldBe Calculator.Error.DigitOutOfRange
    }

    "does nothing" - {
      "when you just repeat pressing `=`" in {
        val calculator = Calculator()
        calculator.calculate.calculate.calculate.calculate.right.value shouldBe calculator
      }

      "when overflows during digits typing" in {
        val calculator = Calculator()
          .enter(2)
          .enter(1)
          .enter(4)
          .enter(7)
          .enter(4)
          .enter(8)
          .enter(3)
          .enter(6)
          .enter(4)
          .enter(7)
          .enter(0)

        calculator.right.value shouldBe Calculator(0, Integer.MAX_VALUE, None)
      }
    }

    "overflows" - {
      "when sum of ints cannot be contained inside int" in {
        val calculator = Calculator()

        calculator.addOvf(Integer.MAX_VALUE, 1).left.value shouldBe Calculator.Error.Overflow
        calculator.addOvf(Integer.MAX_VALUE, Integer.MAX_VALUE).left.value shouldBe Calculator.Error.Overflow
        calculator.addOvf(Integer.MIN_VALUE, -1).left.value shouldBe Calculator.Error.Overflow
        calculator.addOvf(Integer.MIN_VALUE, Integer.MIN_VALUE).left.value shouldBe Calculator.Error.Overflow
      }

      "when mul of ints cannot be contained inside int" in {
        val calculator = Calculator()

        calculator.mulOvf(1073741824, 2).left.value shouldBe Calculator.Error.Overflow
      }
    }

    "operates properly" - {
      "for plus" in {
        val calculator = Calculator()
          .enter(1)
          .plus
          .enter(2)
          .calculate

        calculator.right.value shouldBe Calculator(3, 0, None)
      }

      "for minus" in {
        val calculator = Calculator()
          .enter(1)
          .minus
          .enter(2)
          .calculate

        calculator.right.value shouldBe Calculator(-1, 0, None)
      }

      "for multiply" in {
        val calculator = Calculator()
          .enter(2)
          .multiply
          .enter(3)
          .calculate

        calculator.right.value shouldBe Calculator(6, 0, None)
      }

      "for div" - {
        "when the last operand is not zero" in {
          val calculator = Calculator()
            .enter(6)
            .divide
            .enter(3)
            .calculate

          calculator.right.value shouldBe Calculator(2, 0, None)
        }

        "when the last operand is zero" in {
          val calculator = Calculator()
            .enter(6)
            .divide
            .enter(0)
            .calculate

          calculator.left.value shouldBe Calculator.Error.DivisionByZero
        }
      }

      "for chain of operations" in {
        // ((12 + 34 - 6) * 2) / 4) - 20) = 0
        val calculator = Calculator()
          .enter(1)
          .enter(2)
          .plus
          .enter(3)
          .enter(4)
          .minus
          .enter(6)
          .multiply
          .enter(2)
          .divide
          .enter(4)
          .minus
          .enter(2)
          .enter(0)
          .calculate

        calculator.right.value shouldBe Calculator(0, 0, None)
      }
    }
  }
}

