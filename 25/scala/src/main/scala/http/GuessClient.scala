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
import common.{BotStrategy, Client, ConsoleStrategy, GameStrategy, TheGame}

import scala.concurrent.ExecutionContext
import common.domain.GameId.decoder
import common.domain.AttemptResult.codec

object GuessClient extends IOApp.Simple {
  // 1. Разобраться с протоколом

  // Реализация потенциально не совсем красивая, т.к. непонятно откуда берётся числа. С другой стороны,
  // здесь отсутсвует абстракция от числа. Т.е. в данном случае guess - это не гессс, gameAttempt, или игровой ход

  // Технически, getNextValue + guess - это и есть move
  // Выходит, что всё что надо сделать - это разбить botMove

  // Мне необходимо обеспечить возможно хранить эти составляющие отдельно, чтобы была возможность их декорировать
  // А складывать вместе на самом последнем этапе
  final case class Move[F[_]](getNext: Option[AttemptResult] => F[Int], guess: Int => F[AttemptResult])
  final case class MoveF[F[_]](getNext: Option[AttemptResult] => F[Int], guess: F[Int => F[AttemptResult]])

  // Можно написать для move что-то вроде функции build, которая будет строить move как F[AttemptResult], т.е.
  // соединять getNext с guess
  def toConsoleMove[F[_]: Sync](move: Move[F]): Move[F] = {
    val getNext = { o: Option[AttemptResult] => Console[F].putStr("Enter your guess: ").as(o) } >=> move.getNext

    val show = common.domain.gameResultShow.show _
    val guess =
      move.guess >=> { result => Console[F].putStrLn(show(result)).as(result) }

    Move(getNext, guess)
  }

  def gameLoop[F[_] : Monad](move: Move[F]): F[AttemptResult] = {
    def loop(attemptResultOpt: Option[AttemptResult]): F[AttemptResult] =
      move.getNext(attemptResultOpt) >>=
        move.guess >>=
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

  def consoleGame[F[_]: Sync](client: common.Client[F]): ReaderT[F, NewGame, AttemptResult] = {
    val game = toConsoleMove(Move(common.ConsoleStrategy.apply, client))
    ReaderT.liftF(gameLoop(game))
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

  def genGame2[F[_] : Monad](guess: Int => F[AttemptResult], getNext: Option[AttemptResult] => F[Int]): F[AttemptResult] = {
    def loop: Option[AttemptResult] => F[AttemptResult] =
      getNext >=> guess >=> { attemptResult => if (attemptResult.gameIsFinished) Monad[F].pure(attemptResult) else loop(Option(attemptResult)) }

    loop(Option.empty)
  }

  // ??? Фактически потенциально можно построить всю игру прямо в ран? Восользовавшись несколькими хелперами
  def genGame3[F[_] : Monad](move: Option[AttemptResult] => F[AttemptResult]): F[AttemptResult] = {
    def loop: Option[AttemptResult] => F[AttemptResult] =
      move >=> { attemptResult => if (attemptResult.gameIsFinished) Monad[F].pure(attemptResult) else loop(Option(attemptResult)) }

    loop(Option.empty)
  }

  def genProgram[F[_] : Monad](settingsService: SettingsService[F], game: NewGame => F[AttemptResult]) =
    settingsService.getSettings >>= game

  def consoleGame[F[_]: Sync](guessF: Kleisli[F, NewGame, Client[F]]): NewGame => F[AttemptResult] = {
    type G[A] = Kleisli[F, NewGame, A]
    val guessG: G[Client[G]] = guessF.map { guess => guess andThen Kleisli.liftF[F, NewGame, AttemptResult] }
    val getNextG: GameStrategy[G] = ConsoleStrategy.apply[F] _ andThen Kleisli.liftF[F, NewGame, Int]
    val guessGDecorated: G[Client[G]] = guessG.map(guess => decoratedGuess(guess))

    // generic game
    genGame(guessGDecorated, getNextG).run
  }

  def consoleGame2[F[_] : Sync](guessF: NewGame => F[Client[F]]): TheGame[F] =
    guessF >=> { guess => genGame2(decoratedGuess(guess), decoratedGetNext(ConsoleStrategy[F])) }

  def consoleGame3[F[_]: Sync](guessF: NewGame => F[Client[F]]) = {
    type G[A] = Kleisli[F, NewGame, A]
    val guessG: G[Client[G]] = Kleisli(guessF).map { _ andThen Kleisli.liftF }
    val getNextG: GameStrategy[G] = ConsoleStrategy[F] andThen Kleisli.liftF
    MoveF[G](getNextG, guessG)
  }

  def botGame2[F[_] : Sync](guessF: NewGame => F[Client[F]]): TheGame[F] = settings =>
    guessF(settings)
      .map { _ andThen StateT.liftF[F, MinMax, AttemptResult] }
      .flatMap { guess =>
        // TODO
        // Это не здесь должно происходить. У меня должна быть возможность сделать игру с декорациями и без!
        // Это должен быть полностью отдельный функционал, который можно добавлять и убирать одной строчкой для
        // для консоли и для бота
        // По сути, это означает что мне нужно приводить и возвращать guess и getNext в виде одной монады.
        // Лучше всего скорее всего строить Game, а потом его декорировать?
        val getNext =
          decoratedGetNext(BotStrategy[F]) >=> { number => Console[StateT[F, MinMax, *]].putStrLn(number.toString).as(number) }

        genGame2(decoratedGuess(guess), getNext).runA(MinMax(settings.min, settings.max))
      }

  // Сколько параметров будет?
  // хост(ip и порт), клиентбилдер (хттп или ws), провайдер игровых сеттингов, гейм билдер на их основе.
  // То что вверху и есть цепочка. Так надо и написать

  // Build generic run ???
  def run: IO[Unit] = {
    val test = ipv4"localhost:9001"
    test.value

    // TODO: Console is being recreated multiple times (everytime it is being accessed)
    // implicit val console = Console[IO]

    // import org.http4s.client.middleware.Logger
    // тут потенциально может захотеться менять клиента не таким образом. А задавать хост с портом,
    // а потом указывать тип клиента - как-то так
    // Попробуем это сделать когда появится ws клиент

    http.Client
      .resource[IO](uri"http://localhost:9001")
      // вот здесь можно создавать move(attempt) вместо игры
      // .map(consoleGame2[IO])
      .map(botGame2[IO])
      .use { game => genProgram(SettingsService.console[IO], game) }
      .void
  }

  //  def run: IO[Unit] = {
//    // TODO: Console is being recreated multiple times (everytime it is being accessed)
//    // implicit val console = Console[IO]
//
//    // import org.http4s.client.middleware.Logger
//    // тут потенциально может захотеться менять клиента не таким образом. А задавать хост с портом,
//    // а потом указывать тип клиента - как-то так
//    http.Client.resource[IO](uri"http://localhost:9001").use { clientF =>
//      // TODO: Сделать консольный принтинг для этой версии
//
//      // Console
//      type G[A] = Kleisli[IO, NewGame, A]
//      val guessG: G[Client[G]] = clientF.map { guess => guess andThen Kleisli.liftF[IO, NewGame, AttemptResult] }
//      val getNextG: GameStrategy[G] = ConsoleStrategy.apply[IO] _ andThen Kleisli.liftF[IO, NewGame, Int]
//      val guessGDecorated: G[Client[G]] = guessG.map(guess => decoratedGuess(guess))
//
//      // generic game
//      val game = genGame(guessGDecorated, getNextG).run
//      genProgram(SettingsService.console[IO], game)
//
//      // Bot
//      // Т.е. в оригинале я получаю стейт монаду, и запускаю её сеттингами из Ридера, т.е. моё клеисли всё ещё внешнее
//      // по отношению к стейту (стейт внутри клеисли), но там я весь геймлуп запускаю с runA, а здесь нет
//      // Т.е. мне гейм нужно построить на стейте (genGame), а потом обернуть это в клеисли. Точне, у меня в клеисли
//      // должна быть завёрнута вся игра а не только стратегия
//      val getNextGBot: GameStrategy[G] = attemptResultOpt =>
//        Kleisli.apply[IO, NewGame, Int] { settings =>
//          // Здесь MinMax на каждой итерации должен передаваться разный
//          val getNext = BotStrategy.apply[IO] andThen { x => x.runA(MinMax(settings.min, settings.max)) }
//          getNext(attemptResultOpt)
//      }
//
//      val getNextGBotDecorated: GameStrategy[G] = decoratedGetNext(getNextGBot) >=> { number => Console[G].putStrLn(number.toString).as(number) }
//
//      val gameB = genGame(guessGDecorated, getNextGBotDecorated).run
//      genProgram(SettingsService.console[IO], gameB)
//
//      // Bot 2
//      // В обоих случаях я просто создаю игру? Типа botGame, consoleGame. Как минимум в этом. Надо подумать
//      type H[A] = StateT[IO, MinMax, A]
//      val fK: IO ~> H = new FunctionK[IO, H] {
//        def apply[A](fa: IO[A]): H[A] =
//          StateT.liftF[IO, MinMax, A](fa)
//      }
//
//      type I[A] = Kleisli[H, NewGame, A]
//
//      // Мне надо описать game в рамках StateT, а потом сделать run для него и передавать в gen44
//      // Понятно что для выходного результата оно ничего не требует (т.е. мап внизу не обязателен)
//      // Если я найду способ объединить выход guessF с getNext - всё будет проще
//      // val guessF: Kleisli[H, NewGame, Int => H[AttemptResult]] =
//      //   clientF.mapK[H](fK).map(client => client andThen StateT.liftF[IO, MinMax, AttemptResult])
//
//      // Нужно помнить про то что MinMax у меня есть поднможество Settings. Может это поможет
//      // val getNext: Option[AttemptResult] => Kleisli[H, NewGame, Int] = BotStrategy.apply[IO] andThen Kleisli.liftF[H, NewGame, Int]
//
//      // getNext >=> guess - is move. Maybe can be used
//      // val test: Kleisli[IO, NewGame, Option[AttemptResult] => StateT[IO, MinMax, AttemptResult]] = guessF.map (guess => getNext >=> guess)
//
//      // val game = genGame(guessF, getNext)
//
//      // Значит клиент может зависеть только от изначальных сеттингов, он создаётся один раз. В принципе
//      // есть подозрение что не будет ничего страшного, если я буду передавать туда новый стейт - оно не должно
//      // пересоздаваться, т.к. луп гоняет ту штучку которая была создана изначально. Т.е. здесь два варианта реализации
//      // скорее всего.
//      // Внешка всё равно должна оставаться за Kleisli, потому что игра должна принять параметры, потом эти параметры
//      // должны сконвертиться в MinMax. Т.е. мне нужно чтобы принималось NewGame. Что можно ещё сделать?
//      // можно перевести всё в State[IO, NewGame, *]
//
//      type J[A] = StateT[IO, NewGame, A]
//
//      val kToS = new FunctionK[Kleisli[IO, NewGame, *], StateT[IO, NewGame, *]] {
//        def apply[A](fa: Kleisli[IO, NewGame, A]): StateT[IO, NewGame, A] = StateT { s =>
//          fa.run(s).map { a => (s, a) }
//        }
//      }
//
//      // Ещё можно будет понизить в меньшую сторону - т.е. до MinMax, применив сеттинги на месте, если это возможно
//      val guessJ: J[Client[J]] = kToS.apply(clientF).map (client => client andThen StateT.liftF[IO, NewGame, AttemptResult])
//      val getNext: GameStrategy[J] = BotStrategy.apply[IO] andThen { minMaxS =>
//        // val test: StateT[IO, NewGame, Int] = s.[NewGame](s => MinMax(s.min, s.max)) // CONTRAMAP!
//        StateT[IO, NewGame, Int] { s => minMaxS.run(MinMax(s.min, s.max)).map { t => val mm = t._1; (NewGame(mm.min, mm.max, 0), t._2) } }
//      }
//
//      val guessJDecorated: J[Client[J]] = guessJ.map(decoratedGuess[J])
//      val getNextDecorated: GameStrategy[J] = decoratedGetNext(getNext) >=> { number => Console[J].putStrLn(number.toString).as(number) }
//
//      val gameJ = genGame(guessJDecorated, getNextDecorated).runA _
//      genProgram(SettingsService.console[IO], gameJ)
//
//      // Bot 3
//      type K[A] = StateT[IO, MinMax, A]
//      val gameK = { (settings: NewGame) =>
//        val guessK: K[Client[K]] =
//          StateT.liftF[IO, MinMax, Int => IO[AttemptResult]](clientF.run(settings)).map(client => client andThen StateT.liftF[IO, MinMax, AttemptResult])
//
//        val getNextK: GameStrategy[K] = BotStrategy.apply[IO]
//
//        val guessDecorated: K[Client[K]] = guessK.map(decoratedGuess[K])
//        val getNextDecorated: GameStrategy[K] = decoratedGetNext(getNextK) >=> { number => Console[K].putStrLn(number.toString).as(number) }
//
//        genGame(guessDecorated, getNextDecorated).runA(MinMax(settings.min, settings.max))
//      }
//
//      genProgram(SettingsService.console[IO], gameK)
//    }.void
}