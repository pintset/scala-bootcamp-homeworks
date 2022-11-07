package http

import cats.data.StateT
import cats.{Monad, ~>}
import cats.arrow.FunctionK
import cats.effect.{Concurrent, IO, IOApp, Sync}
import cats.implicits.catsSyntaxFunction1FlatMap
import org.http4s.Uri
import client.SettingsService
import org.http4s.client.dsl.Http4sClientDsl
import common.domain.{AttemptResult, GameId, Greater, Guess, Lower, NewGame}
import org.http4s._
import io.circe.generic.auto._
import org.http4s.circe.CirceEntityCodec._
import effects.Console
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.implicits.http4sLiteralsSyntax
import cats.syntax.functor._
import cats.syntax.flatMap._
import common.BotStrategy.MinMax

import scala.concurrent.ExecutionContext
import common.domain.GameId.decoder
import common.domain.AttemptResult.codec

object GuessClient extends IOApp.Simple {
  // 1. Разобраться с протоколом

  // Реализация потенциально не совсем красивая, т.к. непонятно откуда берётся числа. С другой стороны,
  // здесь отсутсвует абстракция от числа. Т.е. в данном случае guess - это не гессс, gameAttempt, или игровой ход

  // Технически, getNextValue + guess - это и есть move
  // Выходит, что всё что надо сделать - это разбить botMove
  def gameLoop[F[_] : Monad](getNextValue: Option[AttemptResult] => F[Int], guess: Int => F[AttemptResult]): F[AttemptResult] = {
    def loop(attemptResultOpt: Option[AttemptResult]): F[AttemptResult] =
      getNextValue(attemptResultOpt) >>= guess >>= { attemptResult => if (attemptResult.gameIsFinished) Monad[F].pure(attemptResult) else loop(Option(attemptResult)) }

    loop(None)
  }

  def consoleGameLoop[F[_]: Sync](gameLoop: (Option[AttemptResult] => F[Int], Int => F[AttemptResult]) => F[AttemptResult]) = {
    def consoleGameLoop(getNextValue: Option[AttemptResult] => F[Int], guess: Int => F[AttemptResult]): F[AttemptResult] = {
      def newGetNextValue(attemptResultOpt: Option[AttemptResult]) =
        // Нужно только заврапать getNextValue - т.е. это тоже цепочка
        // Можно доработать не guess, а getNextValue
        Console[F].putStr("Enter your guess: ") >> getNextValue(attemptResultOpt)

      val show = common.domain.gameResultShow.show _
      val newGuess = guess >=> { result => Console[F].putStrLn(show(result)).as(result) }

      gameLoop(newGetNextValue, newGuess)
    }

    consoleGameLoop _
  }

  // TODO: Вернуть здесь монаду и сделать по-нормальному
  // def botGame[F[_]: Monad](min: Int, max: Int)(guess: Int => F[AttemptResult]): F[AttemptResult] = {
  // min, max заменить на сеттингс? // Таже самая игра получилась :))
  // def botGame[F[_]: Sync](min: Int, max: Int)(guess: Int => F[AttemptResult]): F[AttemptResult] = {
  def botGame[F[_]: Sync](settings: NewGame)(client: common.Client[F]): F[AttemptResult] = {

    // F ~> G or FunctionK[F, G]
    def passThrough[A](fa: F[A]): StateT[F, MinMax, A] =
      StateT { s: MinMax =>
        fa.map(a => (s, a))
      }

//    val passThrough: F ~> G = new FunctionK[F, G] {
//      def apply[A](fa: F[A]): G[A] = StateT { s =>
//        fa.map(a => (s, a))
//      }
//    }

    val printNext = { (number: Int) => Console[F].putStrLn(number.toString).as(number) } andThen passThrough
    val getNext: Option[AttemptResult] => StateT[F, MinMax, Int] = common.BotStrategy.apply.getNext _ >=> printNext
    val guessG = client.guess _ andThen passThrough

    // Тогда нужно принимать gameLoop как параметр :(
    val newGameLoop = consoleGameLoop(gameLoop[StateT[F, MinMax, *]])
    newGameLoop(getNext, guessG).runA(MinMax(settings.min, settings.max))
  }

  def consoleGame[F[_]: Sync](settings: NewGame)(client: common.Client[F]): F[AttemptResult] = {
    val newGameLoop = consoleGameLoop(gameLoop[F])
    newGameLoop(common.ConsoleStrategy.apply.getNext, client.guess)
  }

  def genProgram[F[_] : Concurrent](client: org.http4s.client.Client[F], host: Uri, settingsService: SettingsService[F],  game: NewGame => common.Client[F] => F[AttemptResult]) = {
    settingsService.getSettings >>= { settings =>
      // Это можно всё в функциях оставить
      http.Client(client, host, settings) >>= game(settings)
    }
  }

  def run: IO[Unit] = {
    // TODO: Console is being recreated multiple times (everytime it is being accessed)
    // implicit val console = Console[IO]

    // import org.http4s.client.middleware.Logger

    // type G[A] = StateT[IO, GameState, A]

    BlazeClientBuilder[IO](ExecutionContext.global).resource.use { client =>
      // consoleProgram(Logger(logHeaders = false, logBody = true)(client), uri"http://localhost:9001")

      // consoleProgram[IO](startReqMemoized[IO](client, uri"http://localhost:9001"), SettingsService.console[IO], consoleBot[IO])

      // val guessGameF: G[Int => IO[AttemptResult]] = passThrough(startReqMemoized[IO](client, uri"http://localhost:9001"))
      // consoleProgram[G](guessGameF, SettingsService.console[IO], consoleBot[IO])

      // botProgram(startReqMemoized[IO](client, uri"http://localhost:9001"), SettingsService.console[IO], botBot[IO])

      // consoleProgram2(client, uri"http://localhost:9001", SettingsService.console[IO])

      // Вот здесь genProgram можно сделать из двух функций. Первая принимает первые одинаковые параметры, вторая
      // последний разный. Нужно попробовать исключить урл например. Урл нам нужен только для клиента
      // genProgram(client, uri"http://localhost:9001", SettingsService.console[IO], consoleGame[IO])
      genProgram(client, uri"http://localhost:9001", SettingsService.console[IO], botGame[IO])
    }.void
  }
}