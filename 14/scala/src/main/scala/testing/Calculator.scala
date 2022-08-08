package testing

import Calculator._

/** Simple calculator with buttons.
  *
  * @param memory whatever is stored in the memory.
  * @param screen whatever you see on the screen.
  */
case class Calculator(memory: Int = 0, screen: Int = 0, operation: Option[Operation] = None) {

  private[testing] def addOvf(a: Integer, b: Integer): Either[Error, Integer] =
    if ((b > 0 && a > Integer.MAX_VALUE - b) || (b < 0 && a < Integer.MIN_VALUE - b)) Left(Error.Overflow) else Right(a + b)

  private[testing] def mulOvf(a: Integer, b: Integer): Either[Error, Integer] = {
    if (a == 0 || b == 0) Right(0)
    else {
      val res = a * b
      if (a == res / b) Right(res) else Left(Error.Overflow)
    }
  }

  def enter(digit: Int): Either[Error, Calculator] =
    if (digit >= 0 && digit <= 9)
      Right(mulOvf(screen, 10).flatMap(addOvf(_, digit)).map(s => this.copy(screen = s)).getOrElse(this))
    else
      Left(Error.DigitOutOfRange)

  private def applyOp(operation: Operation): Either[Error, Calculator] =
    (this.operation match {
      case Some(_) => this.calculate
      case _ => Right(Calculator(memory = screen))
    }).map(c => c.copy(operation = Some(operation)))

  def plus: Either[Error, Calculator] = applyOp(Operation.Plus)
  def minus: Either[Error, Calculator] = applyOp(Operation.Minus)
  def multiply: Either[Error, Calculator] = applyOp(Operation.Multiply)
  def divide: Either[Error, Calculator] = applyOp(Operation.Divide)

  def calculate: Either[Error, Calculator] = operation.fold(Right(this).asInstanceOf[Either[Error, Calculator]]) {
    case Operation.Plus => addOvf(memory, screen).map(m => Calculator(memory = m))
    case Operation.Minus => addOvf(memory, -screen).map(m => Calculator(memory = m))
    case Operation.Multiply => mulOvf(memory, screen).map(m => Calculator(memory = m))
    case Operation.Divide =>
      if (screen == 0) Left(Error.DivisionByZero) else Right(Calculator(memory = memory / screen))
  }
}

object Calculator {
  sealed trait Error
  object Error {
    final case object Overflow extends Error
    final case object DivisionByZero extends Error
    final case object DigitOutOfRange extends Error
  }

  sealed trait Operation
  object Operation {
    object Plus extends Operation
    object Minus extends Operation
    object Multiply extends Operation
    object Divide extends Operation
  }

  implicit class CalculatorOps(c: Either[Error, Calculator]) {
    def enter(digit: Int): Either[Error, Calculator] = c.flatMap(_.enter(digit))
    def plus: Either[Error, Calculator] = c.flatMap(_.plus)
    def minus: Either[Error, Calculator] = c.flatMap(_.minus)
    def multiply: Either[Error, Calculator] = c.flatMap(_.multiply)
    def divide: Either[Error, Calculator] = c.flatMap(_.divide)
    def calculate: Either[Error, Calculator] = c.flatMap(_.calculate)
  }
}
