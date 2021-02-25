package example

import scala.io.StdIn

object Main extends App {

  sealed trait Command

  object Command {

    final case class Divide(dividend: Double, divisor: Double) extends Command

    final case class Sum(numbers: List[Double]) extends Command

    final case class Average(numbers: List[Double]) extends Command

    final case class Min(numbers: List[Double]) extends Command

    final case class Max(numbers: List[Double]) extends Command

  }

  final case class ErrorMessage(value: String)

  object ErrorMessage {
    def apply[R](msg: String): Either[ErrorMessage, R] = Left(ErrorMessage(msg))
  }

  final case class Result(command: Command, value: Double)

  object Number {
    def unapply(x: String): Option[Double] = x.toDoubleOption
  }

  object Numbers {
    def unapply(xs: List[String]): Option[List[Double]] =
      xs.flatMap(Number.unapply) match {
        case Nil => None
        case xs => Some(xs)
      }
  }

  def renderDouble(d: Double): String =
    if (d == d.toLong) d.toLong.toString else d.toString

  def concat[A](xs: List[A]) = xs.mkString(" ")

  def renderDoubleList(command: String, ns: List[Double], result: Double): String = {
    s"the $command of ${concat(ns.map(renderDouble))} is ${renderDouble(result)}"
  }

  def isCommand(c: String) = List("divide", "sum", "average", "min", "max").contains(c)

  def parseCommand(x: String): Either[ErrorMessage, Command] =
    x.toLowerCase.split(' ').toList match {
      case List("divide", Number(_), Number(divisor)) if divisor == 0 => ErrorMessage("Divisor cannot be 0")
      case List("divide", Number(dividend), Number(divisor)) => Right(Command.Divide(dividend, divisor))
      case List("divide", Number(_), x) => ErrorMessage(s"Failed to parse divisor '$x'")
      case List("divide", x, Number(_)) => ErrorMessage(s"Failed to parse dividend '$x'")
      case List("divide", n1, n2) => ErrorMessage(s"Failed to parse divisor '$n1' and dividend '$n2'")
      case "divide" :: ns if ns.length != 2 => ErrorMessage(s"'divide' command requires dividend and divisor specified")

      case "sum" :: Numbers(ns) => Right(Command.Sum(ns))
      case "average" :: Numbers(ns) => Right(Command.Average(ns))
      case "min" :: Numbers(ns) => Right(Command.Min(ns))
      case "max" :: Numbers(ns) => Right(Command.Max(ns))

      case List(command) if isCommand(command) => ErrorMessage(s"$command command requires at least one number as an argument")
      case command :: numbers if isCommand(command) => ErrorMessage(s"Failed to parse numbers '${concat(numbers)}'")
      case List("") => ErrorMessage(s"No command specified")
      case command :: _ => ErrorMessage(s"Unknown command '$command'")

      case xs => ErrorMessage(s"Failed to parse line: '${concat(xs)}'")
    }

  def calculate(x: Command): Result = Result(
    x,
    x match {
      case Command.Divide(dividend, divisor) => dividend / divisor
      case Command.Sum(ns) => ns.sum
      case Command.Average(ns) => ns.sum / ns.length
      case Command.Min(ns) => ns.min
      case Command.Max(ns) => ns.max
    }
  )

  def renderResult(x: Result): String = x match {
    case Result(Command.Divide(dividend, divisor), r) => s"${renderDouble(dividend)} divided by ${renderDouble(divisor)} is ${renderDouble(r)}"
    case Result(Command.Sum(ns), r) => renderDoubleList("sum", ns, r)
    case Result(Command.Average(ns), r) => renderDoubleList("average", ns, r)
    case Result(Command.Min(ns), r) => renderDoubleList("minimum", ns, r)
    case Result(Command.Max(ns), r) => renderDoubleList("maximum", ns, r)
  }

  // Why this will not compile? (Unapplied methods are only converted to functions when a function type is expected.)
  //  def process(x: String): String =
  //    parseCommand(x).map(calculate andThen renderResult) match {
  //      case Left(ErrorMessage(msg)) => msg
  //      case Right(msg) => msg
  //    }

  def process(x: String): String =
    parseCommand(x).map(x => renderResult(calculate(x))) match {
      case Left(ErrorMessage(msg)) => s"Error: $msg"
      case Right(msg) => msg
    }

  Iterator
    .continually(Option(StdIn.readLine()))
    .takeWhile(_.nonEmpty)
    .foreach { x => x map process foreach println }
}
