package testing

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Gen
import org.scalatest.EitherValues
import testing.Calculator.Error

object CalculatorTest {
  val calc: Either[Error, Calculator] = Right(Calculator())

  val intsFrom0: Gen[Int] = Gen.choose(0, Integer.MAX_VALUE)
  val intsFrom0WoOverflow: Gen[Int] = Gen.choose(0, Math.sqrt(Integer.MAX_VALUE).toInt)
  val intsFrom10: Gen[Int] = Gen.choose(10, Integer.MAX_VALUE)
  val smallInts: Gen[Int] = Gen.choose(1, 100)

  implicit val numToDigits: Int => IndexedSeq[Int] = { _.toString.map(_.asDigit) }

  implicit class CalculatorTestOps(c: Either[Error, Calculator]) {
    def tEnter(digits: IndexedSeq[Int]): Either[Error, Calculator] = {
      digits.foldLeft(c){ (c, digit) => c.enter(digit) }
    }
  }
}

import CalculatorTest._

object CalculatorAdditionSpec extends Properties("addition") with EitherValues {
  property("oracle") = forAll(intsFrom0WoOverflow, intsFrom0WoOverflow) { (x: Int, y: Int) =>
    calc.tEnter(x).plus.tEnter(y).calculate.right.value.memory == x + y
  }

  property("identity") = forAll(intsFrom0) { x: Int =>
    calc.tEnter(x).plus.enter(0).calculate.right.value.memory == x
  }

  property("associativity") = forAll(intsFrom0, intsFrom0, intsFrom0) { (x: Int, y: Int, z: Int) =>
    val result1 = for {
      xPlusY <- calc.tEnter(x).plus.tEnter(y).calculate
      plusZ <- calc.tEnter(xPlusY.memory).plus.tEnter(z).calculate
    } yield plusZ

    val result2 = for {
      yPlusZ <- calc.tEnter(y).plus.tEnter(z).calculate
      plusX <- calc.tEnter(x).plus.tEnter(yPlusZ.memory).calculate
    } yield plusX

    result1 == result2
  }

  property("commutivity") = forAll(intsFrom0, intsFrom0) { (x: Int, y: Int) =>
    calc.tEnter(x).plus.tEnter(y).calculate == calc.tEnter(y).plus.tEnter(x).calculate
  }
}

object CalculatorSubtractionSpec extends Properties("subtraction") with EitherValues {
  property("oracle") = forAll(intsFrom0WoOverflow, intsFrom0WoOverflow) { (x: Int, y: Int) =>
    calc.tEnter(x).minus.tEnter(y).calculate.right.value.memory == x - y
  }

  property("identity") = forAll(intsFrom0) { x: Int =>
    calc.tEnter(x).minus.enter(0).calculate == calc.tEnter(x).minus.calculate
  }

  property("as addition") = forAll(intsFrom0WoOverflow, intsFrom0WoOverflow) { (x: Int, y: Int) =>
    calc.tEnter(x).plus.tEnter(y).minus.tEnter(y).calculate.right.value.memory == x
  }
}

object CalculatorMultiplicationSpec extends Properties("multiplication") with EitherValues {
  property("oracle") = forAll(intsFrom0WoOverflow, intsFrom0WoOverflow) { (x: Int, y: Int) =>
    calc.tEnter(x).multiply.tEnter(y).calculate.right.value.memory == x * y
  }

  property("identity") = forAll(intsFrom0) { x: Int =>
    calc.tEnter(x).multiply.enter(1).calculate.right.value.memory == x
  }

  property("associativity") = forAll(intsFrom0, intsFrom0, intsFrom0) { (x: Int, y: Int, z: Int) =>
    val result1 = for {
      xMulY <- calc.tEnter(x).multiply.tEnter(y).calculate
      mulZ <- calc.tEnter(xMulY.memory).multiply.tEnter(z).calculate
    } yield mulZ

    val result2 = for {
      yMulZ <- calc.tEnter(y).multiply.tEnter(z).calculate
      mulX <- calc.tEnter(x).multiply.tEnter(yMulZ.memory).calculate
    } yield mulX

    result1 == result2
  }

  property("commutivity") = forAll(intsFrom0, intsFrom0) { (x: Int, y: Int) =>
    calc.tEnter(x).multiply.tEnter(y).calculate == calc.tEnter(y).multiply.tEnter(x).calculate
  }
}

object CalculatorDivisionSpec extends Properties("division") with EitherValues {
  property("oracle") = forAll(intsFrom0WoOverflow, intsFrom0WoOverflow) { (x: Int, y: Int) =>
    calc.tEnter(x).divide.tEnter(y).calculate.right.value.memory == x / y
  }

  property("identity") = forAll(intsFrom0) { x: Int =>
    calc.tEnter(x).divide.enter(1).calculate.right.value.memory == x
  }

  property("as multiplication") = forAll(intsFrom0WoOverflow, intsFrom0WoOverflow) { (x: Int, y: Int) =>
    calc.tEnter(x).multiply.tEnter(y).divide.tEnter(y).calculate.right.value.memory == x
  }

  property("by zero") = forAll(intsFrom0) { x: Int =>
    calc.tEnter(x).divide.enter(0).calculate.left.value == Calculator.Error.DivisionByZero
  }
}

object CalculatorMiscSpec extends Properties("calculator") with EitherValues {
  property("enter.digit out of range") = forAll(intsFrom10) { x: Int =>
    Calculator().enter(x).left.value == Calculator.Error.DigitOutOfRange
  }

  property("calculate.does nothing when called repeatedly") = forAll(intsFrom0WoOverflow) { x: Int =>
    List.range(0, x).foldRight(calc) { (_, c) => c.calculate } == calc
  }

  property("chain of ops works propertly") = forAll(smallInts, smallInts, smallInts) { (x: Int, y: Int, z: Int) =>
    ((x + y) * z - z * y) / x ==
      calc.tEnter(x).plus.tEnter(y).multiply.tEnter(z)
        .minus.tEnter(calc.tEnter(z).multiply.tEnter(y).calculate.right.value.memory)
        .divide.tEnter(x).calculate.right.value.memory
  }
}
