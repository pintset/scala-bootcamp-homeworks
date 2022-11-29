import cats.data.StateT
import cats.{Monad, Show}
import cats.effect.std.Console
import cats.effect.{Async, IO, IOApp, Sync}
import cats.implicits.catsSyntaxFunction1FlatMap
import client.{GameClient, SettingsService}
import org.http4s.implicits.http4sLiteralsSyntax
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.syntax.applicativeError._
import cats.syntax.traverse._
import client.strategies.ConsoleStrategy
import client.types.{Game, GameClient, Move}
import common.domain.{AttemptResult, NewGame, TryAgain}
import client._
import common.WordsDb
import io.circe.{Decoder, Encoder}

object ClientApp extends IOApp.Simple {
  def decorateMove[F[_]: Sync](wordsDb: WordsDb[F])(move: Move[F, String])(implicit c: Console[F]): Move[StateT[F, List[String], *], String] = {
    val getNextG: StateT[F, List[String], String] = StateT.inspectF { results =>
      def loop: F[String] = results.traverse(c.println) >> c.print("Enter your guess: ") >> move.getNext >>=
        { word => wordsDb.isValidWord(word).ifM(Sync[F].pure(word), c.println("Please try another word") >> loop) }

      loop
    }

    implicit val show: Show[AttemptResult[String]] = common.domain.gameResultShow

    val updateState: AttemptResult[String] => StateT[F, List[String], Unit] = result =>
      StateT.modify[F, List[String]] { prevResults =>
        result match {
          case r @ TryAgain(_, _) => prevResults.appended(show.show(r))
          case _ => prevResults
        }
      }

    val guessG: String => StateT[F, List[String], AttemptResult[String]] =
      move.guess.map(StateT.liftF[F, List[String], AttemptResult[String]])

    val guess: String => StateT[F, List[String], AttemptResult[String]] =
      guessG >=> { result =>
        val f =
          result match {
            case TryAgain(_, _) => Sync[F].unit
            case r => c.println(r)
          }

        StateT.liftF[F, List[String], Unit](f) >> updateState(result).as(result)
      }

    Move(getNextG, guess)
  }

//  implicit class RepeatOps[F[_]: Monad, A](fa: F[A]) {
//    def repeatWhile(predicate: A => Boolean): F[A] =
//      fa.flatMap { a => if (predicate(a)) repeatWhile(predicate) else Monad[F].pure(a) }
//  }

  def gameLoop[F[_]: Monad, A](move: Move[F, A]): F[AttemptResult[A]] = {
    def loop: F[AttemptResult[A]] =
      move.getNext >>= move.guess >>= { attemptResult => if (attemptResult.gameIsFinished) Monad[F].pure(attemptResult) else loop }

    loop
  }

  def consoleGame[F[_]: Sync: Console](wordsDb: WordsDb[F])(guessF: NewGame[String] => F[GameClient[F, String]]): Game[F, String] =
    guessF >=> (ConsoleStrategy.move[F] _ andThen decorateMove[F](wordsDb)).map { move =>
      gameLoop[StateT[F, List[String], *], String](move).runA(List.empty)
    }

  def program[F[_]: Async, A: Encoder: Decoder](gameBuilder: (NewGame[A] => F[GameClient[F, A]]) => Game[F, A]): F[Unit] = {
    implicit val console: Console[F] = Console.make[F]

    val settingsService = SettingsService[F, A]
    // val settingsService = SettingsService.console

    services.Http.resource[F, A](uri"http://localhost:9001")
    // services.Ws.resource[F, A](uri"ws://localhost:9001")
      .map { GameClient[F, A] _ andThen gameBuilder }
      .use { game => settingsService.getSettings >>= game }
      .void
  }

  def run: IO[Unit] = {
    // implicit val stringCodec = Codec.from(Decoder[String], Encoder[String])
    WordsDb.make[IO](WordsDb.validWords) >>= { wordsDb => program[IO, String](consoleGame[IO](wordsDb)) }
  }
}