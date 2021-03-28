package error_handling

import cats.Show
import cats.data.Validated.Invalid
import cats.data.ValidatedNec
import cats.syntax.all._
import error_handling.Homework.PaymentCardValidator.AllErrorsOr

import java.time.{Year, YearMonth}
import java.time.format.DateTimeFormatter

// Homework. Place the solution under `error_handling` package in your homework repository.
//
// 1. Model `PaymentCard` class as an ADT (protect against invalid data as much as it makes sense).
// 2. Add `ValidationError` cases (at least 5, may be more).
// 3. Implement `validate` method to construct `PaymentCard` instance from the supplied raw data.
object Homework extends App {
  final case class PaymentCard private (name: String, number: String, expirationDate: YearMonth, securityCode: String)

  object PaymentCard {
    def apply(name: String, number: String, expirationDate: String, securityCode: String): AllErrorsOr[PaymentCard] =
      PaymentCardValidator.validate(name, number, expirationDate, securityCode)
  }

  sealed trait ValidationError
  object ValidationError {
    final case object CardNameHasInvalidCharacters extends ValidationError {
      override def toString: String = "Card holder name contains invalid characters"
    }

    final case object CardNameHasInvalidLength extends ValidationError {
      override def toString: String = "Card holder name length should be from 1 to 48 characters"
    }

    final case object CardNumberHasInvalidCharacters extends ValidationError {
      override def toString: String = "Card number is allowed to contain only digits and space characters"
    }

    final case object CardNumberHasInvalidLength extends ValidationError {
      override def toString: String = "Card number length can be from 13 to 16 digits"
    }

    final case object CardNumberHasInvalidChecksum extends ValidationError {
      override def toString: String = "Card number has invalid checksum"
    }

    final case object CardExpirationDateHasInvalidFormat extends ValidationError {
      override def toString: String = "Card expiration date has invalid format"
    }

    final case object CardExpirationMonthCanContainOnlyDigits extends ValidationError {
      override def toString: String = "Card expiration month should contain digits only"
    }

    final case object CardExpirationYearCanContainOnlyDigits extends ValidationError {
      override def toString: String = "Card expiration year should contain digits only"
    }

    final case object CardExpirationMonthIsOutOfRange extends ValidationError {
      override def toString: String = "Card expiration month should be in range 01 - 12"
    }

    final case object CardExpirationYearIsOutOfRange extends ValidationError {
      override def toString: String = "Card expiration year should be in range 00 - 99"
    }

    final case object CardSecurityCodeCanContainOnlyDigits extends ValidationError {
      override def toString: String = "Card security code should contain digits only"
    }

    final case object CardSecurityCodeHasInvalidLength extends ValidationError {
      override def toString: String = "Card security code length is invalid. Should be 3 or 4 digits (depends on issuer)"
    }
  }

  object PaymentCardValidator {

    type AllErrorsOr[A] = ValidatedNec[ValidationError, A]

    def validate(
                  name: String,
                  number: String,
                  expirationDate: String,
                  securityCode: String,
                ): AllErrorsOr[PaymentCard] = {

      def validateName: AllErrorsOr[String] = {
        def validateLength: AllErrorsOr[String] =
          if (name.nonEmpty && name.length <= 48) name.validNec
          else ValidationError.CardNameHasInvalidLength.invalidNec

        def validateChars(name: String): AllErrorsOr[String] =
          if ("^[a-zA-Z\\s'`~.\\-]{1,48}$".r.matches(name)) name.validNec
          else ValidationError.CardNameHasInvalidCharacters.invalidNec

        validateLength.andThen(validateChars)
      }

      def validateNumber: AllErrorsOr[String] = {
        def validateChars: AllErrorsOr[String] = {
          val n = number.split("\\s+").mkString
          if (n.forall(_.isDigit)) n.validNec
          else ValidationError.CardNumberHasInvalidCharacters.invalidNec
        }

        def validateLength(number: String): AllErrorsOr[String] =
          if (number.length >= 13 && number.length <= 16) number.validNec
          else ValidationError.CardNumberHasInvalidLength.invalidNec

        def validateNumber(number: String): AllErrorsOr[String] = {
          val sumOfDigits =
            number.map(_.asDigit).reverse.zipWithIndex
              .map { case (d, i) =>
                val n = d * (if (i % 2 == 0) 1 else 2)
                n / 10 + n % 10
              }.sum

          if (sumOfDigits % 10 == 0) number.validNec else ValidationError.CardNumberHasInvalidChecksum.invalidNec
        }

        validateChars.andThen(validateLength).andThen(validateNumber)
      }

      def validateExpirationDate: AllErrorsOr[YearMonth] = {
        def validateMonth(month: String): AllErrorsOr[Int] = {
          def areDigits: AllErrorsOr[Int] =
            month.toIntOption.toValidNec(ValidationError.CardExpirationMonthCanContainOnlyDigits)

          def validRange(month: Int): AllErrorsOr[Int] =
            if (month >= 1 && month <= 12) month.validNec
            else ValidationError.CardExpirationMonthIsOutOfRange.invalidNec

          areDigits.andThen(validRange)
        }

        def validateYear(year: String): AllErrorsOr[String] = {
          def areDigits: AllErrorsOr[Int] =
            year.toIntOption.toValidNec(ValidationError.CardExpirationYearCanContainOnlyDigits)

          def validRange(year: Int): AllErrorsOr[Int] =
            if (year >= 0 && year <= 99) year.validNec
            else ValidationError.CardExpirationYearIsOutOfRange.invalidNec

          areDigits.andThen(validRange).map(_ => year)
        }

        val YEAR_FORMAT = DateTimeFormatter.ofPattern("[yyyy][yy]") // Can convert 4 digits year too
        expirationDate.split('/').map(_.strip) match {
          case Array(month, year) => (validateMonth(month), validateYear(year)).mapN { case (month, year) =>
            YearMonth.of(Year.parse(year, YEAR_FORMAT).getValue, month) }
          case _ => ValidationError.CardExpirationDateHasInvalidFormat.invalidNec
        }
      }

      def validateSecurityCode(number: String): AllErrorsOr[String] = {
        def areDigits: AllErrorsOr[String] = {
          val code = securityCode.strip
          if (code.forall(_.isDigit)) code.validNec else ValidationError.CardSecurityCodeCanContainOnlyDigits.invalidNec
        }

        def validateLength(code: String): AllErrorsOr[String] = {
          val length = if (number.length == 15) 4 else 3
          if (code.length == length) code.validNec else ValidationError.CardSecurityCodeHasInvalidLength.invalidNec
        }

        areDigits.andThen(validateLength)
      }

//      val valSecCode: AllErrorsOr[String] = validateNumber.andThen(validateSecurityCode)
//      (validateName, validateNumber, validateExpirationDate, valSecCode).mapN { case (name, number, expirationDate, securityCode) =>
//        PaymentCard(name, number, expirationDate, securityCode)
//      }

      (validateName, validateNumber, validateExpirationDate).mapN { case (name, number, expirationDate) =>
        validateSecurityCode(number).map { code =>
          PaymentCard(name, number, expirationDate, code)
        }
      }.andThen(identity)
    }
  }

  implicit val stringShow: Show[ValidationError] = t => t.toString

  val visa = PaymentCard("DENIS TERLETSKIY", "4024 0071 4041 3922", "03/21", "544")
  val americanExpress = PaymentCard("DENIS TERLETSKIY", "3781 495977 58944", "03/21", "7997")

  val messages =
    Seq(visa, americanExpress).map {
      case Invalid(errs) => errs.mkString_("\n")
      case card => card.toString
    }

  println(messages)
}

// Attributions and useful links:
// https://www.lihaoyi.com/post/StrategicScalaStylePrincipleofLeastPower.html#error-handling
// https://www.geeksforgeeks.org/scala-exception-handling/
// https://typelevel.org/cats/datatypes/validated.html
// https://blog.ssanj.net/posts/2019-08-18-using-validated-for-error-accumulation-in-scala-with-cats.html
