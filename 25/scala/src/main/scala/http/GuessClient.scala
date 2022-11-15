package http

import cats.data.{IndexedStateT, Kleisli, Reader, ReaderT, StateT}
import cats.{Applicative, Id, Monad, ~>}
import cats.arrow.FunctionK
import cats.effect.{Concurrent, ConcurrentEffect, IO, IOApp, Sync}
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
import common.BotStrategy.MoveState
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
  final case class Move[F[_]](getNext: F[Int], guess: Int => F[AttemptResult])
  final case class MoveF[F[_]](getNext: F[Int], guess: F[Int => F[AttemptResult]])

  // Можно написать для move что-то вроде функции build, которая будет строить move как F[AttemptResult], т.е.
  // соединять getNext с guess
  def toConsoleMove[F[_]: Sync](move: Move[F]): Move[F] = {
    val getNext = Console[F].putStr("Enter your guess: ") >> move.getNext

    val show = common.domain.gameResultShow.show _
    val guess =
      move.guess >=> { result => Console[F].putStrLn(show(result)).as(result) }

    Move(getNext, guess)
  }

  def gameLoop[F[_] : Monad](move: Move[F]): F[AttemptResult] = {
    def loop: F[AttemptResult] =
      move.getNext>>=
        move.guess >>=
        { attemptResult => if (attemptResult.gameIsFinished) Monad[F].pure(attemptResult) else loop }

    loop
  }

  // Можно ещё вместо этого сделать консоль game, и передать его в gameLoop. Тогда не нужен будет consoleGameLoop
  def decoratedGetNext[F[_]: Sync](getNext: GameStrategy[F]): GameStrategy[F] =
    Console[F].putStr("Enter your guess: ") >> getNext

  def decoratedGuess[F[_]: Sync](guess: Client[F]): Client[F] = {
    val show = common.domain.gameResultShow.show _
    guess >=> { result => Console[F].putStrLn(show(result)).as(result) }
  }

  def printNext[F[_]: Sync]: Int => F[Int] = { (number: Int) => Console[F].putStrLn(number.toString).as(number) }

  // А клиента со стратегией можно сделать консольными до подачи сюда
  // Теперь достаточно передать туда монады - и всё заработать должно
  def genGame[F[_] : Monad](guessF: F[Int => F[AttemptResult]], getNext: F[Int]): F[AttemptResult] = {
    guessF >>= { guess =>
      def loop: F[AttemptResult] =
        getNext >>= guess >>= { attemptResult => if (attemptResult.gameIsFinished) Monad[F].pure(attemptResult) else loop }

      loop
    }
  }

  implicit class RepeatOps[F[_]: Monad, A](fa: F[A]) {
    def repeatWhile(predicate: A => Boolean): F[A] =
      fa.flatMap { a => if (predicate(a)) repeatWhile(predicate) else Monad[F].pure(a) }
  }

  // Запихнуть ли Option[AttemptResult] в стейт? Надо прикинуть такую версию
  def genGame2[F[_] : Monad](guess: Int => F[AttemptResult], getNext: F[Int]): F[AttemptResult] = {
    def loop: F[AttemptResult] =
      getNext >>= guess >>= { attemptResult => if (attemptResult.gameIsFinished) Monad[F].pure(attemptResult) else loop }

    loop
  }

  def genGame22[F[_] : Monad](move: Move[F]): F[AttemptResult] = {
    def loop: F[AttemptResult] =
      move.getNext >>= move.guess >>= { attemptResult => if (attemptResult.gameIsFinished) Monad[F].pure(attemptResult) else loop }

    loop
  }

  // ??? Фактически потенциально можно построить всю игру прямо в ран? Восользовавшись несколькими хелперами
  def genGame3[F[_] : Monad](move: F[AttemptResult]): F[AttemptResult] = {
    def loop: F[AttemptResult] =
      move >>= { attemptResult => if (attemptResult.gameIsFinished) Monad[F].pure(attemptResult) else loop }

    loop
  }

  def genProgram[F[_] : Monad](settingsService: SettingsService[F], game: NewGame => F[AttemptResult]) =
    settingsService.getSettings >>= game

  def consoleGame[F[_]: Sync](guessF: Kleisli[F, NewGame, Client[F]]): NewGame => F[AttemptResult] = {
    type G[A] = Kleisli[F, NewGame, A]
    val guessG: G[Client[G]] = guessF.map { guess => guess andThen Kleisli.liftF[F, NewGame, AttemptResult] }
    val getNextG: GameStrategy[G] = Kleisli.liftF[F, NewGame, Int](ConsoleStrategy[F])
    val guessGDecorated: G[Client[G]] = guessG.map(guess => decoratedGuess(guess))

    // generic game
    genGame(guessGDecorated, getNextG).run
  }

  def consoleGame2[F[_] : Sync](guessF: NewGame => F[Client[F]]): TheGame[F] =
    guessF >=> { guess => genGame2(decoratedGuess(guess), decoratedGetNext(ConsoleStrategy[F])) }

  def consoleGame3[F[_]: Sync](guessF: NewGame => F[Client[F]], strategy: GameStrategy[F]): TheGame[F] =
    guessF >=> { guess => genGame2(guess, strategy) }

  // genGame можно сделать как функцию, принимающую клиента, и возвращающую то что принимает стратегию
  // Должно быть красивее
  def consoleGame4[F[_] : Sync](guessF: NewGame => F[Client[F]], strategy: GameStrategy[F]): TheGame[F] =
    guessF.map { x => genGame(x, strategy) }

  def botGame2[F[_] : Sync](guessF: NewGame => F[Client[F]]): TheGame[F] = settings =>
    guessF(settings)
      .map { clientF =>
        (number: Int) => StateT { move: MoveState =>
          clientF(number).map { r => (move.copy(attemptResultOpt = Option(r)), r) }
        }
      }
      .flatMap { guess =>
        // TODO
        // Это не здесь должно происходить. У меня должна быть возможность сделать игру с декорациями и без!
        // Это должен быть полностью отдельный функционал, который можно добавлять и убирать одной строчкой для
        // для консоли и для бота
        // По сути, это означает что мне нужно приводить и возвращать guess и getNext в виде одной монады.
        // Лучше всего скорее всего строить Game, а потом его декорировать?
        val getNext =
          decoratedGetNext(BotStrategy[F]) >>= { number => Console[StateT[F, MoveState, *]].putStrLn(number.toString).as(number) }

        genGame2(decoratedGuess(guess), getNext).runA(MoveState(settings.min, settings.max, None))
      }

  def toBotClient[F[_]: Monad](guess: Int => F[AttemptResult]) =
    guess.map { fa => StateT { move: MoveState => fa.map { a => (move.copy(attemptResultOpt = Option(a)), a) } } }

  def botGame3[F[_] : Sync](guessF: NewGame => F[Client[F]], strategy: GameStrategy[StateT[F, MoveState, *]]): TheGame[F] = settings =>
    guessF(settings)
      // .map { _.map { fa => StateT { move: MoveState => fa.map { a => (move.copy(attemptResultOpt = Option(a)), a) } } } }
      .map(toBotClient[F])
      .flatMap { guess => genGame2(guess, strategy).runA(MoveState(settings.min, settings.max, None)) }

  def botGame4[F[_] : Sync](guessF: NewGame => F[Client[F]], strategy: GameStrategy[StateT[F, MoveState, *]]): TheGame[F] =  {
    type G[A] = StateT[F, MoveState, A]

    val f: NewGame => G[AttemptResult] =
      guessF
        .map { fClientF => StateT.liftF[F, MoveState, Client[G]](fClientF.map { clientF =>
          (number: Int) =>
            StateT { move: MoveState =>
              clientF(number).map { r => (move.copy(attemptResultOpt = Option(r)), r) }
            }
        }) }
        .map { gClientG => genGame(gClientG, strategy) }

    settings => f(settings).runA(MoveState(settings.min, settings.max, None))
  }

  def consoleMove[F[_]: Sync](guess: Int => F[AttemptResult]): Move[F] =
    Move(ConsoleStrategy[F], guess)

  def botMove[F[_]: Monad](guess: Int => F[AttemptResult]): Move[StateT[F, MoveState, *]] =
    Move(BotStrategy[F], toBotClient(guess))

  def botMoveF[F[_] : Monad](guessF: F[Int => F[AttemptResult]]): F[Move[StateT[F, MoveState, *]]] =
    guessF.map { guess => Move(BotStrategy[F], toBotClient(guess)) }

//  def botMove[F[_]: Applicative](guess: Int => F[AttemptResult]) = {
//    val botGuess = guess andThen StateT.liftF[F, MoveState, AttemptResult]
//    Move(BotStrategy[F], botGuess)
//  }

  // Сколько параметров будет?
  // хост(ip и порт), клиентбилдер (хттп или ws), провайдер игровых сеттингов, гейм билдер на их основе.
  // То что вверху и есть цепочка. Так надо и написать

//  def genRun1[F[_] : Concurrent : ConcurrentEffect] = {
//    http.Client
//      .resource[F](uri"http://localhost:9001")
//      .map { guessF =>
//        settings: NewGame =>
//          guessF(settings)
//            //.map(consoleMove)
//            // Тогда возникает проблема в том что сеттинги нужно дёргать - т.е. вылазит проблема сеттингов
//            // из-за того что toConsoleMove - разделен
//            .map(botMove)
//            // Возникает разница в последнем флатмапе - можно правда попробовать это тоже куда-то вынести
//            // Т.е. всё в принципе хорошо, кроме того что toConsoleMove торчит в середине
//            // если сместить это в начало самое - будет сильно лучше. Тогда я смогу объединить
//            // move с нижним флатмапом и получить F
//            .map(toConsoleMove)
//
//            //.flatMap(genGame22)
//            .flatMap(genGame22(_).runA(MinMax(settings.min, settings.max)))
//        }
//  }

  // TODO: Почему нельзя построить тип Game? Ведь сеттинги есть

  def genRun2[F[_] : Concurrent : ConcurrentEffect] = {
    http.Client
      .resource[F](uri"http://localhost:9001")
      .map { guessF =>
        val guessFDecorated = guessF.andThen(_.map(decoratedGuess[F]))
        // val strategyDecorated = decoratedGetNext(ConsoleStrategy[F])
        val strategyDecorated =
        // На это забъём короче
          decoratedGetNext(BotStrategy[F]) >>= { number => Console[StateT[F, MoveState, *]].putStrLn(number.toString).as(number) }

        // consoleGame3(guessFDecorated, strategyDecorated)
        botGame3(guessFDecorated, strategyDecorated)
      }
      .use { game => genProgram(SettingsService.console[F], game) }
      .void
  }

  def genRun[F[_] : Concurrent : ConcurrentEffect] = {
    http.Client
      .resource[F](uri"http://localhost:9001")

      .map { guessF =>
        guessF.map { fClientF =>

          fClientF
//            .map { guess => Move(ConsoleStrategy[F], guess) }
             .map(toBotClient andThen BotStrategy.move)

//            .map { move =>
//              val getNext = decoratedGetNext(move.getNext : GameStrategy[F])
//              val guess = decoratedGuess(move.guess)
//
//              Move(getNext, guess)
//            }

            .map { move =>
              val getNext =
                decoratedGetNext(move.getNext) >>= { number => Console[StateT[F, MoveState, *]].putStrLn(number.toString).as(number) }

              val guess = decoratedGuess(move.guess)

              Move(getNext, guess)
            }

            //.flatMap(genGame22)
        }
      }

      // Game
//      .map { guessF =>
//        (s: NewGame) => guessF(s) >>= genGame22 }
//      }
      // Проблема в том что у меня тут всё равно торчит вот этот ран
      .map { guessF =>
        (s: NewGame) => guessF(s) >>= { move => genGame22(move).runA(MoveState(s.min, s.max, None)) }
      }

      // Fire
      .use { game => genProgram(SettingsService.console[F], game) }
      .void
  }

  def run: IO[Unit] = genRun[IO]

//  // Build generic run ???
//  def run: IO[Unit] = {
//    val test = ipv4"localhost:9001"
//    test.value
//
//    // TODO: Console is being recreated multiple times (everytime it is being accessed)
//    // implicit val console = Console[IO]
//
//    // import org.http4s.client.middleware.Logger
//    // тут потенциально может захотеться менять клиента не таким образом. А задавать хост с портом,
//    // а потом указывать тип клиента - как-то так
//    // Попробуем это сделать когда появится ws клиент
//
//    http.Client
//      .resource[IO](uri"http://localhost:9001")
//      // вот здесь можно создавать move(attempt) вместо игры
//      // .map(consoleGame2[IO])
//      .map(botGame2[IO])
//      .use { game => genProgram(SettingsService.console[IO], game) }
//      .void
//  }

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