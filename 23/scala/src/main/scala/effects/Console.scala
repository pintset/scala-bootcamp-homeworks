package effects

import cats.data.StateT
import cats.effect.Sync
import cats.implicits.catsSyntaxFlatMapOps
import cats.syntax.applicativeError._
import cats.{Applicative, ~>}

import scala.io.StdIn

trait Console[F[_]] { self =>
  def getInt: F[Int]
  def putStr(message: String): F[Unit]
  def putStrLn(message: String): F[Unit]

  def mapK[G[_]](f: F ~> G): Console[G] = new Console[G] {
    def getInt: G[Int] = f(self.getInt)
    def putStr(message: String): G[Unit] = f(self.putStr(message))
    def putStrLn(message: String): G[Unit] = f(self.putStrLn(message))
  }
}

object Console {
  def apply[F[_]: Console]: Console[F] = implicitly

  def make[F[_]: Sync]: Console[F] = new Console[F] {
    def getInt: F[Int] = Sync[F].delay(StdIn.readInt())
    def putStr(message: String): F[Unit] = Sync[F].delay(print(message))
    def putStrLn(message: String): F[Unit] = Sync[F].delay(println(message))
  }

  def inputInt[F[_]: Sync](message: String)(implicit c: Console[F]): F[Int] =
    c.putStr(message) >> c.getInt

  def inputIntWithRetry[F[_]: Sync](message: String)(implicit c: Console[F]): F[Int] =
    inputInt(message).handleErrorWith { _ =>
      c.putStrLn("Failed to parse your input. Please try again") >> inputIntWithRetry(message)
    }

  implicit def stateConsole[F[_]: Console: Applicative, S]: Console[StateT[F, S, *]] = Console[F].mapK(StateT.liftK)
}
