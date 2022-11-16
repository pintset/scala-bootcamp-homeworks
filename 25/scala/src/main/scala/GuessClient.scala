import cats.data.StateT
import cats.Monad
import cats.effect.{Concurrent, ConcurrentEffect, IO, IOApp, Sync}
import cats.implicits.catsSyntaxFunction1FlatMap
import client.SettingsService
import common.domain.{AttemptResult, Move, NewGame}
import effects.Console
import org.http4s.implicits.http4sLiteralsSyntax
import cats.syntax.functor._
import cats.syntax.flatMap._
import common.BotStrategy.MoveState
import common.{BotStrategy, Client, ConsoleStrategy, GameStrategy, TheGame}

object GuessClient extends IOApp.Simple {
  // TODO: Разобраться с протоколом

  // Можно ещё вместо этого сделать консоль game, и передать его в gameLoop. Тогда не нужен будет consoleGameLoop
  def decorateGetNext[F[_]: Sync](getNext: GameStrategy[F]): GameStrategy[F] =
    Console[F].putStr("Enter your guess: ") >> getNext

  def decorateGuess[F[_]: Sync](guess: Client[F]): Client[F] = {
    val show = common.domain.gameResultShow.show _
    guess >=> { result => Console[F].putStrLn(show(result)).as(result) }
  }

  def decorateMove[F[_] : Sync](move: Move[F]): Move[F] = {
    val getNext = decorateGetNext(move.getNext: GameStrategy[F])
    val guess = decorateGuess(move.guess)

    Move(getNext, guess)
  }

  implicit class RepeatOps[F[_]: Monad, A](fa: F[A]) {
    def repeatWhile(predicate: A => Boolean): F[A] =
      fa.flatMap { a => if (predicate(a)) repeatWhile(predicate) else Monad[F].pure(a) }
  }

  def gameLoop[F[_] : Monad](move: Move[F]): F[AttemptResult] = {
    def loop: F[AttemptResult] =
      move.getNext >>= move.guess >>= { attemptResult => if (attemptResult.gameIsFinished) Monad[F].pure(attemptResult) else loop }

    loop
  }

  def consoleGame[F[_] : Sync](guessF: NewGame => F[Client[F]]): TheGame[F] =
    guessF >=> (ConsoleStrategy.move[F] _ andThen decorateMove[F]).map(gameLoop[F])

  //    guessF.map {
  //      _
  //        .map(ConsoleStrategy.move[F] _ andThen decorateMove[F])
  //        .flatMap(genGame22[F])
  //    }

  def botGame[F[_] : Sync](guessF: NewGame => F[Client[F]]): TheGame[F] = settings =>
    guessF(settings)
      .map(BotStrategy.move[F] _ andThen decorateMove[StateT[F, MoveState, *]])
      .flatMap { move =>
        val m =
          move.copy(getNext = move.getNext >>= { number => Console[StateT[F, MoveState, *]].putStrLn(number.toString).as(number) })

        gameLoop(m).runA(MoveState(settings.min, settings.max, None))
      }

  def consoleGamePure[F[_] : Sync](guessF: NewGame => F[Client[F]]): TheGame[F] =
    guessF >=> (ConsoleStrategy.move[F] _).map(gameLoop[F])

  def botGamePure[F[_] : Sync](guessF: NewGame => F[Client[F]]): TheGame[F] = settings =>
    guessF(settings)
      .map(BotStrategy.move[F])
      .flatMap { move => gameLoop(move).runA(MoveState(settings.min, settings.max, None)) }

  // Сколько параметров будет?
  // хост(ip и порт), клиентбилдер (хттп или ws), провайдер игровых сеттингов, гейм билдер на их основе.
  // То что вверху и есть цепочка. Так надо и написать
  def program[F[_] : Concurrent : ConcurrentEffect] = {
    val settingsService = SettingsService.console
    // val createGame = botGame22[F] _
    val gameBuilder = consoleGame[F] _

    http.Client
      .resource[F](uri"http://localhost:9001")
      .map(gameBuilder)
      .use { game => settingsService.getSettings >>= game }
      .void
  }

  def run: IO[Unit] = program[IO]
}