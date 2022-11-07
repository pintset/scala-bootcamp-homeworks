import cats.Monad
import cats.effect.{Concurrent, IO, IOApp, Sync}
import cats.syntax.flatMap._
import cats.syntax.functor._

object TestApp extends IOApp.Simple {
  def putStr[F[_]: Sync](message: String): F[Unit] = Sync[F].delay(println(message))

  def someRequest[F[_]: Sync]: F[Int] =
    putStr("Performing request...") >> Sync[F].pure(5)

  def computation[F[_]: Concurrent]: F[Int] =
    for {
      memoizedRequest <- Concurrent.memoize(someRequest)
      _ <- putStr("This should happen before request")
      result1 <- memoizedRequest
      result2 <- memoizedRequest.map {
        _ * 2
      }
    } yield result1 + result2

  def genericProgram[F[_]: Monad](showMessage: String => F[Unit])(request: F[Int]): F[Int] =
    showMessage("This should happen before request") >> request >>= { result1 => request.map { r => r * 2 }.map { result2 => result1 + result2 } }

  def memoizedRequest[F[_]: Concurrent]: F[F[Int]] = Concurrent.memoize(someRequest)

  def program2[F[_]: Concurrent]: F[Unit] = memoizedRequest >>= genericProgram(putStr[F]) >>= { result => putStr(s"Result: $result") }

  def program[F[_]: Concurrent]: F[Unit] =
    computation >>= { result => putStr(s"Result: $result") }

  def run: IO[Unit] = program2[IO]
}
