import cats.data.StateT
import cats.Monad
import cats.effect.{Concurrent, ConcurrentEffect, ContextShift, IO, IOApp, Sync}
import cats.implicits.catsSyntaxFunction1FlatMap
import client.SettingsService
import effects.Console
import org.http4s.implicits.http4sLiteralsSyntax
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.syntax.applicativeError._
import ch.qos.logback.classic.LoggerContext
import client._
import client.strategies.BotStrategy.MoveState
import client.strategies.{BotStrategy, ConsoleStrategy}
import client.types.{Game, GameClient, Move}
import common.domain.{AttemptResult, NewGame}
import org.slf4j.LoggerFactory

object ClientApp extends IOApp.Simple {
  def decorateMove[F[_]: Sync](move: Move[F])(implicit c: Console[F]): Move[F] = {
    def getNext: F[Int] =
      { c.putStr("Enter your guess: ") >> move.getNext }
        .handleErrorWith { _ => c.putStrLn("Failed to parse your input. Please try again") >> getNext }

    val show = common.domain.gameResultShow.show _
    val guess = move.guess >=> { result => c.putStrLn(show(result)).as(result) }

    Move(getNext, guess)
  }

  def gameLoop[F[_] : Monad](move: Move[F]): F[AttemptResult] = {
    def loop: F[AttemptResult] =
      move.getNext >>= move.guess >>= { attemptResult => if (attemptResult.gameIsFinished) Monad[F].pure(attemptResult) else loop }

    loop
  }

  def consoleGame[F[_]: Sync: Console](guessF: NewGame => F[GameClient[F]]): Game[F] =
    guessF >=> (ConsoleStrategy.move[F] _ andThen decorateMove[F]).map(gameLoop[F])

  def botGame[F[_]: Sync: Console](guessF: NewGame => F[GameClient[F]]): Game[F] = settings =>
    guessF(settings)
      .map(BotStrategy.move[F] _ andThen decorateMove[StateT[F, MoveState, *]])
      .flatMap { move =>
        val m =
          move.copy(getNext = move.getNext >>= { number => Console.apply[StateT[F, MoveState, *]].putStrLn(number.toString).as(number) })

        gameLoop(m).runA(MoveState(settings.min, settings.max, None))
      }

  def consoleGamePure[F[_]: Sync: Console](guessF: NewGame => F[GameClient[F]]): Game[F] =
    guessF >=> (ConsoleStrategy.move[F] _).map(gameLoop[F])

  def botGamePure[F[_] : Sync](guessF: NewGame => F[GameClient[F]]): Game[F] = settings =>
    guessF(settings)
      .map(BotStrategy.move[F])
      .flatMap { move => gameLoop(move).runA(MoveState(settings.min, settings.max, None)) }

  def program[F[_]: Concurrent: ContextShift: ConcurrentEffect: Console]: F[Unit] = {
    // val settingsService = SettingsService[F]
    val settingsService = SettingsService.console
    // val gameBuilder = botGame[F] _
    val gameBuilder = consoleGame[F] _

    GameClient.resource[F](uri"http://localhost:9001")
      .map { gameBuilder }
      .use { game => settingsService.getSettings >>= game }
      .void
  }

  def run: IO[Unit] = {
    LoggerFactory.getILoggerFactory.asInstanceOf[LoggerContext].stop()
    implicit val console = Console.make[IO]

    program[IO]
  }
}