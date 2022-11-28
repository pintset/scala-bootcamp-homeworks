package common

import cats.Show
import cats.effect.Sync
import cats.effect._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.applicativeError._

object Console {
  def readInt[F[_]: Sync](implicit c: std.Console[F]): F[Int] =
    c.readLine.map(_.toInt)

  def inputInt[F[_]: Sync, A](a: A)(implicit c: std.Console[F], s: Show[A]): F[Int] =
    c.print(a) >> readInt

  def inputIntWithRetry[F[_]: Sync, A](a: A)(implicit c: std.Console[F], s: Show[A]): F[Int] =
    inputInt(a).handleErrorWith { _ =>
      c.println("Failed to parse your input. Please try again") >> inputIntWithRetry(a)
    }
}