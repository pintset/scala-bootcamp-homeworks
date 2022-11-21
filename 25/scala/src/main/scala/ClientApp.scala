import cats.data.StateT
import cats.Monad
import cats.effect.{Concurrent, ConcurrentEffect, ContextShift, IO, IOApp, Sync}
import cats.implicits.catsSyntaxFunction1FlatMap
import client.SettingsService
import effects.Console
import org.http4s.implicits.http4sLiteralsSyntax
import cats.syntax.functor._
import cats.syntax.flatMap._
import client.clients._
import client.strategies.BotStrategy.MoveState
import client.strategies.{BotStrategy, ConsoleStrategy}
import client.types.{Client, Game, Move}
import common.domain.{AttemptResult, NewGame}

object ClientApp extends IOApp.Simple {
  def decorateMove[F[_] : Sync](move: Move[F]): Move[F] = {
    val getNext = Console[F].putStr("Enter your guess: ") >> move.getNext

    val show = common.domain.gameResultShow.show _
    val guess = move.guess >=> { result => Console[F].putStrLn(show(result)).as(result) }

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

  def consoleGame[F[_] : Sync](guessF: NewGame => F[Client[F]]): Game[F] =
    guessF >=> (ConsoleStrategy.move[F] _ andThen decorateMove[F]).map(gameLoop[F])

  //    guessF.map {
  //      _
  //        .map(ConsoleStrategy.move[F] _ andThen decorateMove[F])
  //        .flatMap(genGame22[F])
  //    }

  def botGame[F[_] : Sync](guessF: NewGame => F[Client[F]]): Game[F] = settings =>
    guessF(settings)
      .map(BotStrategy.move[F] _ andThen decorateMove[StateT[F, MoveState, *]])
      .flatMap { move =>
        val m =
          move.copy(getNext = move.getNext >>= { number => Console[StateT[F, MoveState, *]].putStrLn(number.toString).as(number) })

        gameLoop(m).runA(MoveState(settings.min, settings.max, None))
      }

  def consoleGamePure[F[_] : Sync](guessF: NewGame => F[Client[F]]): Game[F] =
    guessF >=> (ConsoleStrategy.move[F] _).map(gameLoop[F])

  def botGamePure[F[_] : Sync](guessF: NewGame => F[Client[F]]): Game[F] = settings =>
    guessF(settings)
      .map(BotStrategy.move[F])
      .flatMap { move => gameLoop(move).runA(MoveState(settings.min, settings.max, None)) }

  // Сколько параметров будет?
  // хост(ip и порт), клиентбилдер (хттп или ws), провайдер игровых сеттингов, гейм билдер на их основе.
  // То что вверху и есть цепочка. Так надо и написать
  def program[F[_] : Concurrent : ContextShift : ConcurrentEffect] = {
    val settingsService = SettingsService[F]
    // val settingsService = SettingsService.console
    // val createGame = botGame22[F] _
    val gameBuilder = consoleGame[F] _

    // HttpClient.resource[F](uri"http://localhost:9001")
    WsClient.resource[F](uri"ws://localhost:9001")
      .map(gameBuilder)
      .use { game => settingsService.getSettings >>= game }
      .void
  }

  def run: IO[Unit] = program[IO]
}