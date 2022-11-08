package http

import cats.data.{Reader, ReaderT, StateT}
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
import common.Client

import scala.concurrent.ExecutionContext
import common.domain.GameId.decoder
import common.domain.AttemptResult.codec

object GuessClient extends IOApp.Simple {
  // 1. Разобраться с протоколом

  // Реализация потенциально не совсем красивая, т.к. непонятно откуда берётся числа. С другой стороны,
  // здесь отсутсвует абстракция от числа. Т.е. в данном случае guess - это не гессс, gameAttempt, или игровой ход

  // Технически, getNextValue + guess - это и есть move
  // Выходит, что всё что надо сделать - это разбить botMove
  final case class Game[F[_]](getNextValue: Option[AttemptResult] => F[Int], guess: Int => F[AttemptResult])

  def gameLoop[F[_] : Monad](game: Game[F]): F[AttemptResult] = {
    def loop(attemptResultOpt: Option[AttemptResult]): F[AttemptResult] =
      game.getNextValue(attemptResultOpt) >>=
        game.guess >>=
        { attemptResult => if (attemptResult.gameIsFinished) Monad[F].pure(attemptResult) else loop(Option(attemptResult)) }

    loop(None)
  }

  // Можно ещё вместо этого сделать консоль game, и передать его в gameLoop. Тогда не нужен будет consoleGameLoop
  def toConsoleGame[F[_]: Sync](game: Game[F]): Game[F] = {
    val getNextValue =
      { o: Option[AttemptResult] => Console[F].putStr("Enter your guess: ").as(o) } >=> game.getNextValue

    val show = common.domain.gameResultShow.show _
    val guess =
      game.guess >=> { result => Console[F].putStrLn(show(result)).as(result) }

    Game(getNextValue, guess)
  }

//  def consoleGameLoop[F[_]: Sync](gameLoop: Game[F] => F[AttemptResult]): Game[F] => F[AttemptResult] = {
//    def consoleGameLoop(game: Game[F]): F[AttemptResult] = {
//      def newGetNextValue(attemptResultOpt: Option[AttemptResult]) =
//        // Нужно только заврапать getNextValue - т.е. это тоже цепочка
//        // Можно доработать не guess, а getNextValue
//        Console[F].putStr("Enter your guess: ") >> game.getNextValue(attemptResultOpt)
//
//      val show = common.domain.gameResultShow.show _
//      val newGuess = game.guess >=> { result => Console[F].putStrLn(show(result)).as(result) }
//
//      gameLoop(Game(newGetNextValue, newGuess))
//    }
//
//    consoleGameLoop
//  }

  // TODO: Вернуть здесь монаду и сделать по-нормальному
  // def botGame[F[_]: Monad](min: Int, max: Int)(guess: Int => F[AttemptResult]): F[AttemptResult] = {
  // min, max заменить на сеттингс? // Таже самая игра получилась :))
  // def botGame[F[_]: Sync](min: Int, max: Int)(guess: Int => F[AttemptResult]): F[AttemptResult] = {
  def botGame[F[_]: Sync](settings: NewGame)(client: common.Client[F]): F[AttemptResult] = {
    // F ~> G or FunctionK[F, G] where G[A] = StateT[F, MinMax, A]
    def passThrough[A] = StateT.liftF[F, MinMax, A] _

//    def passThrough[A](fa: F[A]): StateT[F, MinMax, A] = {
//      StateT { s: MinMax =>
//        fa.map(a => (s, a))
//      }
//    }

    //    val passThrough: F ~> G = new FunctionK[F, G] {
//      def apply[A](fa: F[A]): G[A] = StateT { s =>
//        fa.map(a => (s, a))
//      }
//    }

    val printNext = { (number: Int) => Console[F].putStrLn(number.toString).as(number) } andThen passThrough
    val getNext: Option[AttemptResult] => StateT[F, MinMax, Int] = common.BotStrategy.apply.getNext _ >=> printNext
    val guessG = client.guess _ andThen passThrough

    // Тогда нужно принимать gameLoop как параметр :(
    // val newGameLoop = consoleGameLoop(gameLoop[StateT[F, MinMax, *]])
    val game = toConsoleGame(Game(getNext, guessG))
    gameLoop(game).runA(MinMax(settings.min, settings.max))
  }

  def consoleGame[F[_]: Sync](settings: NewGame)(client: common.Client[F]): F[AttemptResult] = {
    val game = toConsoleGame(Game(common.ConsoleStrategy.apply.getNext, client.guess))
    gameLoop(game)
  }

  def genProgram[F[_] : Concurrent](settingsService: SettingsService[F],  getClient: NewGame => F[Client[F]], game: NewGame => common.Client[F] => F[AttemptResult]) = {
    settingsService.getSettings >>= { settings =>
      // Это можно всё в функциях оставить. Можно совместить две функции без сеттингов, и потом передать одинаковый параметр
      // ReaderT ??? (NewSettings)

      // это просто что-то что принимает settings, и это что-то хочется объединить вместе. А потом просто подсунуть
      // и написать: settingsService >>= "это что-то"
      getClient(settings) >>= game(settings)
    }
  }

  def run: IO[Unit] = {
    // TODO: Console is being recreated multiple times (everytime it is being accessed)
    // implicit val console = Console[IO]

    // import org.http4s.client.middleware.Logger
    // тут потенциально может захотеться менять клиента не таким образом. А задавать хост с портом,
    // а потом указывать тип клиента - как-то так
    http.Client.resource[IO](uri"http://localhost:9001").use { client =>
      // Вот здесь genProgram можно сделать из двух функций. Первая принимает первые одинаковые параметры, вторая
      // последний разный. Нужно попробовать исключить урл например. Урл нам нужен только для клиента
      // genProgram(client, uri"http://localhost:9001", SettingsService.console[IO], consoleGame[IO])
      genProgram(SettingsService.console[IO], client, botGame[IO])
    }.void
  }
}