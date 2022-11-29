package common

import cats.syntax.functor._
import io.circe.{Codec, Decoder, Encoder}
import server.GameResult

import java.util.UUID
import scala.io.AnsiColor

object domain {
  final case class GameId(uuid: UUID) extends AnyVal
  object GameId {
    import cats.effect.Sync
    import effects.GenUUID

    implicit val encoder: Encoder[GameId] = Encoder[UUID].contramap[GameId](gameId => gameId.uuid)
    implicit val decoder: Decoder[GameId] = Decoder[UUID].map(uuid => GameId(uuid))

    def generate[F[_]: Sync](implicit idGen: GenUUID[F]): F[GameId] = idGen.createUUID.map(GameId(_))
  }

  sealed trait GameAction[A]
  final case class NewGame[A](attemptCount: Int) extends GameAction[A]
  final case class Guess[A](gameId: GameId, guess: A) extends GameAction[A]

  object GameAction {
    import io.circe.generic.auto._
    import io.circe.syntax._

    implicit def encoder[A: Encoder]: Encoder[GameAction[A]] = Encoder.instance {
      case newGame @ NewGame(_) => newGame.asJson
      case guess @ Guess(_, _) => guess.asJson
    }

    implicit def decoder[A: Decoder]: Decoder[GameAction[A]] = Decoder[NewGame[A]].widen or Decoder[Guess[A]].widen
  }

  // TODO: enum кодек какой нибудь
  sealed trait LetterPosition
  case object Correct extends LetterPosition
  case object Incorrect extends LetterPosition
  case object Missing extends LetterPosition

  object LetterPosition {
    import io.circe.generic.extras.Configuration
    import io.circe.generic.extras.semiauto.deriveConfiguredCodec

    implicit val genDevConfig: Configuration =
      Configuration.default.withDiscriminator("letterPosition")

    implicit val codec: Codec[LetterPosition] = deriveConfiguredCodec[LetterPosition]
  }

  sealed trait AttemptResult[A] {
    def gameIsFinished: Boolean = this match {
      case YouWon(_, _) | GameOver(_) => true
      case _ => false
    }
  }

  object AttemptResult {
    import io.circe.generic.extras.Configuration
    import io.circe.generic.extras.semiauto.deriveConfiguredCodec

    implicit val genDevConfig: Configuration =
      Configuration.default.withDiscriminator("attemptResult")

    implicit def codec[A: Encoder: Decoder]: Codec[AttemptResult[A]] = deriveConfiguredCodec[AttemptResult[A]]
  }

  import cats.Show
  import scala.io.AnsiColor._
  final val BRIGHT_WHITE = "\u001b[97m"
  implicit val gameResultShow: Show[AttemptResult[String]] = {
    case YouWon(attemptsUsed, guess) => s"You won. You used $attemptsUsed attempt(s) to guess $guess"
    case GameOver(answer) => s"You lost. Correct answer is $answer"
    case TryAgain(guess, mask) =>
      BRIGHT_WHITE + BOLD + mask.zipWithIndex.map { case (p, i) =>
        p match {
          case Correct => GREEN_B + guess(i)
          case Incorrect => YELLOW_B + guess(i)
          case Missing => BLACK_B + guess(i)
        }
      }.mkString + RESET
  }

  final case class YouWon[A](attemptsUsed: Int, guess: A) extends AttemptResult[A]
  final case class GameOver[A](answer: A) extends AttemptResult[A]
  final case class TryAgain[A](guess: A, mask: List[LetterPosition]) extends AttemptResult[A]

  final case class ErrorResponse(error: String)

  final case class GameNotFound(gameId: GameId)
  object GameNotFound {
    import io.circe.generic.auto._

    implicit val encoder: Encoder[GameNotFound] = Encoder[ErrorResponse].contramap { gameNotFound =>
      ErrorResponse(s"There is no game with id: ${gameNotFound.gameId.uuid}")
    }
  }

  implicit val gameResult: GameResult[String] = (game, guess) => {
    def buildMask(answer: String, guess: String): List[LetterPosition] = {
      val x = answer.map(Option(_)).zipWithIndex
      guess.zipWithIndex
        .foldLeft((x, List.empty[LetterPosition])) { (state, item) =>
          val answer = state._1
          val mask = state._2
          val l = item._1
          val i = item._2

          if (answer(i)._1.contains(l)) (answer.updated(i, (None, i)), Correct :: mask)
          else {
            val j = answer.indexWhere { case (x, i) => !answer(i)._1.contains(guess(i)) && x.contains(l)}
            if (j > 0) (answer.updated(j, (None, j)), Incorrect :: mask)
            else
              (answer, Missing :: mask)
          }
        }._2.reverse
    }


    if (game.answer == guess) YouWon(game.attemptCount - game.attemptsLeft, game.answer)
    else if (game.attemptsLeft == 0) GameOver(game.answer)
    else TryAgain(guess, buildMask(game.answer, guess))
  }
}
