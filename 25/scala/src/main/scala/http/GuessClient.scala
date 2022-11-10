package http

import cats.data.{IndexedStateT, Kleisli, Reader, ReaderT, StateT}
import cats.{Id, Monad, ~>}
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
import common.{BotStrategy, Client, ConsoleStrategy, GameStrategy}

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

  def decoratedGetNext[F[_]: Sync](getNext: GameStrategy[F]): GameStrategy[F] =
    { o: Option[AttemptResult] => Console[F].putStr("Enter your guess: ").as(o) } >=> getNext

  def decoratedGuess[F[_]: Sync](guess: Client[F]): Client[F] = {
    val show = common.domain.gameResultShow.show _
    guess >=> { result => Console[F].putStrLn(show(result)).as(result) }
  }

  def printNext[F[_]: Sync]: Int => F[Int] = { (number: Int) => Console[F].putStrLn(number.toString).as(number) }

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
  def botGame[F[_]: Sync](guess: common.Client[F]): ReaderT[F, NewGame, AttemptResult] = {
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
    val getNext: Option[AttemptResult] => StateT[F, MinMax, Int] = common.BotStrategy.apply >=> printNext
    val guessG = guess andThen passThrough

    // Тогда нужно принимать gameLoop как параметр :(
    // val newGameLoop = consoleGameLoop(gameLoop[StateT[F, MinMax, *]])
    val game = toConsoleGame(Game(getNext, guessG))

    ReaderT { settings => gameLoop(game).runA(MinMax(settings.min, settings.max)) }
  }

  def consoleGame[F[_]: Sync](client: common.Client[F]): ReaderT[F, NewGame, AttemptResult] = {
    val game = toConsoleGame(Game(common.ConsoleStrategy.apply, client))
    ReaderT.liftF(gameLoop(game))
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

  def genProgram2[F[_]: Monad](getClient: F[Client[F]], game: common.Client[F] => F[AttemptResult]): F[AttemptResult] = {
    // getClient возвращает guess. Теперь мне надо написать generic игру. А generic игра - это гейм луп
    getClient >>= game
  }

  // А клиента со стратегией можно сделать консольными до подачи сюда
  // Теперь достаточно передать туда монады - и всё заработать должно
  def genGame[F[_] : Monad](guessF: F[Int => F[AttemptResult]], getNext: Option[AttemptResult] => F[Int]): F[AttemptResult] = {
    guessF >>= { guess =>
      def loop: Option[AttemptResult] => F[AttemptResult] =
        getNext >=> guess >=> { attemptResult => if (attemptResult.gameIsFinished) Monad[F].pure(attemptResult) else loop(Option(attemptResult)) }

      loop(Option.empty)
    }
  }

  // Create Game. Получается что я на основании сеттинов здесь уже создаю всю игру целиком. Т.е. получаю клиент, и уже его
  // оборачиваю
  // Нужно просто вернуть монадки из функций производителей.
  def genProgram3[F[_]: Monad](getClient: NewGame => F[Client[F]], game: common.Client[F] => NewGame => F[AttemptResult]) = {
    type G[A] = ReaderT[F, NewGame, A]
    val newGetClient: G[Client[F]] = ReaderT(getClient)
    val newGame: Client[F] => G[AttemptResult] = { clientF => ReaderT(game(clientF)) }

    newGetClient.flatMap(newGame).run

    ReaderT(getClient).flatMap(clientF => ReaderT(game(clientF))).run
  }

  def genProgram4[F[_]: Monad](settingsService: SettingsService[F], clientF: ReaderT[F, NewGame, Client[F]], game: common.Client[F] => ReaderT[F, NewGame, AttemptResult]) =
    settingsService.getSettings >>= clientF.flatMap(game).run

  def genProgram44[F[_] : Monad](settingsService: SettingsService[F], game: NewGame => F[AttemptResult]) =
    settingsService.getSettings >>= game

  def run: IO[Unit] = {
    // TODO: Console is being recreated multiple times (everytime it is being accessed)
    // implicit val console = Console[IO]

    // import org.http4s.client.middleware.Logger
    // тут потенциально может захотеться менять клиента не таким образом. А задавать хост с портом,
    // а потом указывать тип клиента - как-то так
    http.Client.resource[IO](uri"http://localhost:9001").use { clientF =>
      // Вот здесь genProgram можно сделать из двух функций. Первая принимает первые одинаковые параметры, вторая
      // последний разный. Нужно попробовать исключить урл например. Урл нам нужен только для клиента
      // genProgram(client, uri"http://localhost:9001", SettingsService.console[IO], consoleGame[IO])
      // genProgram(SettingsService.console[IO], client, botGame[IO])
      // genProgram4(SettingsService.console[IO], client, consoleGame[IO])
      // genProgram4(SettingsService.console[IO], client, botGame[IO])

      // TODO: Сделать консольный принтинг для этой версии

      // Console
      type G[A] = Kleisli[IO, NewGame, A]
      val guessG: G[Client[G]] = clientF.map { guess => guess andThen Kleisli.liftF[IO, NewGame, AttemptResult] }
      val getNextG: GameStrategy[G] = ConsoleStrategy.apply[IO] _ andThen Kleisli.liftF[IO, NewGame, Int]
      val guessGDecorated: G[Client[G]] = guessG.map(guess => decoratedGuess(guess))

      // generic game
      val game = genGame(guessGDecorated, getNextG).run
      genProgram44(SettingsService.console[IO], game)

      // Bot
      // Т.е. в оригинале я получаю стейт монаду, и запускаю её сеттингами из Ридера, т.е. моё клеисли всё ещё внешнее
      // по отношению к стейту (стейт внутри клеисли), но там я весь геймлуп запускаю с runA, а здесь нет
      // Т.е. мне гейм нужно построить на стейте (genGame), а потом обернуть это в клеисли. Точне, у меня в клеисли
      // должна быть завёрнута вся игра а не только стратегия
      val getNextGBot: GameStrategy[G] = attemptResultOpt =>
        Kleisli.apply[IO, NewGame, Int] { settings =>
          // Здесь MinMax на каждой итерации должен передаваться разный
          val getNext = BotStrategy.apply[IO] andThen { x => x.runA(MinMax(settings.min, settings.max)) }
          getNext(attemptResultOpt)
      }

      val getNextGBotDecorated: GameStrategy[G] = decoratedGetNext(getNextGBot) >=> { number => Console[G].putStrLn(number.toString).as(number) }

      val gameB = genGame(guessGDecorated, getNextGBotDecorated).run
      genProgram44(SettingsService.console[IO], gameB)

      // Bot 2
      // В обоих случаях я просто создаю игру? Типа botGame, consoleGame. Как минимум в этом. Надо подумать
      type H[A] = StateT[IO, MinMax, A]
      val fK: IO ~> H = new FunctionK[IO, H] {
        def apply[A](fa: IO[A]): H[A] =
          StateT.liftF[IO, MinMax, A](fa)
      }

      type I[A] = Kleisli[H, NewGame, A]

      // Мне надо описать game в рамках StateT, а потом сделать run для него и передавать в gen44
      // Понятно что для выходного результата оно ничего не требует (т.е. мап внизу не обязателен)
      // Если я найду способ объединить выход guessF с getNext - всё будет проще
      // val guessF: Kleisli[H, NewGame, Int => H[AttemptResult]] =
      //   clientF.mapK[H](fK).map(client => client andThen StateT.liftF[IO, MinMax, AttemptResult])

      // Нужно помнить про то что MinMax у меня есть поднможество Settings. Может это поможет
      // val getNext: Option[AttemptResult] => Kleisli[H, NewGame, Int] = BotStrategy.apply[IO] andThen Kleisli.liftF[H, NewGame, Int]

      // getNext >=> guess - is move. Maybe can be used
      // val test: Kleisli[IO, NewGame, Option[AttemptResult] => StateT[IO, MinMax, AttemptResult]] = guessF.map (guess => getNext >=> guess)

      // val game = genGame(guessF, getNext)

      // Значит клиент может зависеть только от изначальных сеттингов, он создаётся один раз. В принципе
      // есть подозрение что не будет ничего страшного, если я буду передавать туда новый стейт - оно не должно
      // пересоздаваться, т.к. луп гоняет ту штучку которая была создана изначально. Т.е. здесь два варианта реализации
      // скорее всего.
      // Внешка всё равно должна оставаться за Kleisli, потому что игра должна принять параметры, потом эти параметры
      // должны сконвертиться в MinMax. Т.е. мне нужно чтобы принималось NewGame. Что можно ещё сделать?
      // можно перевести всё в State[IO, NewGame, *]

      type J[A] = StateT[IO, NewGame, A]

      val kToS = new FunctionK[Kleisli[IO, NewGame, *], StateT[IO, NewGame, *]] {
        def apply[A](fa: Kleisli[IO, NewGame, A]): StateT[IO, NewGame, A] = StateT { s =>
          fa.run(s).map { a => (s, a) }
        }
      }

      // Ещё можно будет понизить в меньшую сторону - т.е. до MinMax, применив сеттинги на месте, если это возможно
      val guessJ: J[Client[J]] = kToS.apply(clientF).map (client => client andThen StateT.liftF[IO, NewGame, AttemptResult])
      val getNext: GameStrategy[J] = BotStrategy.apply[IO] andThen { minMaxS =>
        // val test: StateT[IO, NewGame, Int] = s.[NewGame](s => MinMax(s.min, s.max)) // CONTRAMAP!
        StateT[IO, NewGame, Int] { s => minMaxS.run(MinMax(s.min, s.max)).map { t => val mm = t._1; (NewGame(mm.min, mm.max, 0), t._2) } }
      }

      val guessJDecorated: J[Client[J]] = guessJ.map(decoratedGuess[J])
      val getNextDecorated: GameStrategy[J] = decoratedGetNext(getNext) >=> { number => Console[J].putStrLn(number.toString).as(number) }

      val gameJ = genGame(guessJDecorated, getNextDecorated).runA _
      genProgram44(SettingsService.console[IO], gameJ)

      // Bot 3
      type K[A] = StateT[IO, MinMax, A]
      val gameK = { (settings: NewGame) =>
        val guessK: K[Client[K]] =
          StateT.liftF[IO, MinMax, Int => IO[AttemptResult]](clientF.run(settings)).map(client => client andThen StateT.liftF[IO, MinMax, AttemptResult])

        val getNextK: GameStrategy[K] = BotStrategy.apply[IO]

        val guessDecorated: K[Client[K]] = guessK.map(decoratedGuess[K])
        val getNextDecorated: GameStrategy[K] = decoratedGetNext(getNextK) >=> { number => Console[K].putStrLn(number.toString).as(number) }

        genGame(guessDecorated, getNextDecorated).runA(MinMax(settings.min, settings.max))
      }

      genProgram44(SettingsService.console[IO], gameK)
    }.void
  }
}