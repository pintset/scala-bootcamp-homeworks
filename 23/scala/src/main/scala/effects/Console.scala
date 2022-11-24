package effects

import cats.effect.Sync
import cats.implicits.catsSyntaxFlatMapOps
import cats.syntax.applicativeError._

import scala.io.StdIn

trait Console[F[_]] {
  def getInt: F[Int]

  def putStr(message: String): F[Unit]
  def putStrLn(message: String): F[Unit]
}

object Console {
  def apply[F[_]: Sync] = new Console[F] {
    def getInt: F[Int] = Sync[F].delay(StdIn.readInt())
    def putStr(message: String): F[Unit] = Sync[F].delay(print(message))
    def putStrLn(message: String): F[Unit] = Sync[F].delay(println(message))
  }

  def inputInt[F[_]: Sync](message: String): F[Int] =
    Console[F].putStr(message) >> Console[F].getInt

  def inputIntWithRetry[F[_] : Sync](message: String): F[Int] =
    inputInt(message).handleErrorWith { _ =>
      Console[F].putStrLn("Failed to parse your input. Please try again") >> inputIntWithRetry(message)
    }
}
