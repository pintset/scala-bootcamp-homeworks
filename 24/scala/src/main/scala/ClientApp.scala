import cats.data.StateT
import cats.{Monad, Show}
import cats.effect.std.Console
import cats.effect.{Async, IO, IOApp, Sync}
import cats.implicits.catsSyntaxFunction1FlatMap
import client.SettingsService
import org.http4s.implicits.http4sLiteralsSyntax
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.syntax.applicativeError._
import client._
import client.strategies.BotStrategy.MoveState
import client.strategies.{BotStrategy, ConsoleStrategy}
import client.types.{Game, GameClient, Move}
import common.domain.{AttemptResult, NewGame}

object ClientApp extends IOApp.Simple {
  def decorateMove[F[_]: Sync](move: Move[F])(implicit c: Console[F]): Move[F] = {
    def getNext: F[Int] = {
      { c.print("Enter your guess: ") >> move.getNext }
        .handleErrorWith { _ => c.println("Failed to parse your input. Please try again") >> getNext }
    }

    implicit val show: Show[AttemptResult] = common.domain.gameResultShow
    val guess = move.guess >=> { result => c.println(result).as(result) }

    Move(getNext, guess)
  }

//  implicit class RepeatOps[F[_]: Monad, A](fa: F[A]) {
//    def repeatWhile(predicate: A => Boolean): F[A] =
//      fa.flatMap { a => if (predicate(a)) repeatWhile(predicate) else Monad[F].pure(a) }
//  }

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
          move.copy(getNext = move.getNext >>= { number => Console[StateT[F, MoveState, *]].println(number).as(number) })

        gameLoop(m).runA(MoveState(settings.min, settings.max, None))
      }

  def consoleGamePure[F[_]: Sync: Console](guessF: NewGame => F[GameClient[F]]): Game[F] =
    guessF >=> (ConsoleStrategy.move[F] _).map(gameLoop[F])

  def botGamePure[F[_] : Sync](guessF: NewGame => F[GameClient[F]]): Game[F] = settings =>
    guessF(settings)
      .map(BotStrategy.move[F])
      .flatMap { move => gameLoop(move).runA(MoveState(settings.min, settings.max, None)) }

  def program[F[_]: Async]: F[Unit] = {
    implicit val console: Console[F] = Console.make[F]

    // val settingsService = SettingsService[F]
    val settingsService = SettingsService.console
    // val gameBuilder = botGame[F] _
    val gameBuilder = consoleGame[F] _

     // services.Http.resource[F](uri"http://localhost:9001")
     services.Ws.resource[F](uri"ws://localhost:9001")
      .map { GameClient[F] _ andThen gameBuilder }
      .use { game => settingsService.getSettings >>= game }
      .void
  }

  def run: IO[Unit] = program[IO]
}