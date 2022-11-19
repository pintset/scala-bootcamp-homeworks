package common

import cats.Show
import cats.effect.Sync
import effects.GenUUID

import java.util.UUID
import cats.syntax.functor._
import io.circe.{Codec, Decoder, Encoder}
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.semiauto.{deriveConfiguredCodec, deriveConfiguredDecoder, deriveConfiguredEncoder}

object domain {
  // Нужно чтобы к классу move можно было добавлять getGame
  final case class Move[F[_]](getNext: F[Int], guess: Int => F[AttemptResult])

  final case class Game(guessedNumber: Int, attemptCount: Int, attemptsLeft: Int) {
    def result(guess: Int): AttemptResult =
      if (guessedNumber == guess) YouWon(attemptCount - attemptsLeft, guessedNumber)
      else if (attemptsLeft == 0) GameOver(guessedNumber)
      else if (guess > guessedNumber) Greater(attemptsLeft)
      else Lower(attemptsLeft)

//    def isOver(guess: Int): Boolean =
//      attemptsLeft == 0 || guessedNumber == guess
  }

  final case class GameId(uuid: UUID) extends AnyVal
  object GameId {
    implicit val encoder: Encoder[GameId] = Encoder[UUID].contramap[GameId](gameId => gameId.uuid)
    implicit val decoder: Decoder[GameId] = Decoder[UUID].map(uuid => GameId(uuid))

    def generate[F[_]: Sync]: F[GameId] = GenUUID[F].createUUID.map(GameId(_))
  }

  sealed trait GameAction
  final case class NewGame(min: Int, max: Int, attemptCount: Int) extends GameAction
  final case class Guess(gameId: GameId, guess: Int) extends GameAction

  object GameAction {
    import io.circe.generic.auto._
    import io.circe.syntax._

    implicit val encoder: Encoder[GameAction] = Encoder.instance {
      case newGame @ NewGame(_, _, _) => newGame.asJson
      case guess @ Guess(_, _) => guess.asJson
    }

    implicit val decoder: Decoder[GameAction] = Decoder[NewGame].widen or Decoder[Guess].widen
  }

  sealed trait AttemptResult {
    def gameIsFinished: Boolean = this match {
      case YouWon(_, _) | GameOver(_) => true
      case _ => false
    }
  }

  object AttemptResult {
    implicit val genDevConfig: Configuration =
      Configuration.default.withDiscriminator("attemptResult")

    implicit val codec: Codec[AttemptResult] = deriveConfiguredCodec[AttemptResult]
  }

  // TODO: Fix attempts in YouWon
  implicit val gameResultShow = new Show[AttemptResult] {
    def show(t: AttemptResult): String = t match {
      case YouWon(attemptsUsed, guess) => s"You won. You used $attemptsUsed attempts to guess number $guess"
      case GameOver(answer) => s"You lost. Correct answer is $answer"
      case Greater(attemptsLeft) => s"Try to guess lower number. You have $attemptsLeft attempts left"
      case Lower(attemptsLeft) => s"Try to guess greater number. You have $attemptsLeft attempts left"
    }
  }

  final case class YouWon(attemptsUsed: Int, guess: Int) extends AttemptResult
  final case class GameOver(answer: Int) extends AttemptResult
  final case class Greater(attemptsLeft: Int) extends AttemptResult
  final case class Lower(attemptsLeft: Int) extends AttemptResult

  // TODO: Remove GameNotFound from GameResult
  final case class ErrorResponse(error: String)

  final case class GameNotFound(gameId: GameId)
  object GameNotFound {
    import io.circe.generic.auto._

    implicit val encoder = Encoder[ErrorResponse].contramap[GameNotFound]{ gameNotFound =>
      ErrorResponse(s"There is no game with id: ${gameNotFound.gameId.uuid}")
    }
  }
}
