package common

import cats.Show
import cats.effect.Sync
import effects.GenUUID

import java.util.UUID
import cats.syntax.functor._
import io.circe.{Codec, Decoder, Encoder}
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.semiauto.deriveConfiguredCodec

object domain {
  final case class Game(guessedNumber: Int, attemptsLeft: Int) {
    def result(guess: Int): AttemptResult =
      if (guessedNumber == guess) YouWon(attemptsLeft, guessedNumber)
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
  final case class Guess(gameId: GameId, number: Int) extends GameAction

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
      case YouWon(usedAttempts, guess) => s"You won. You used $usedAttempts attempts to guess number $guess"
      case GameOver(answer) => s"You lost. Correct answer is $answer"
      case Greater(attemptsLeft) => s"Try to guess lower number. You have $attemptsLeft attempts left"
      case Lower(attemptsLeft) => s"Try to guess greater number. You have $attemptsLeft attempts left"
      // case GameNotFound(gameId: GameId) => s"Game with id $gameId was not found"
    }
  }

  final case class YouWon(usedAttempts: Int, guess: Int) extends AttemptResult
  final case class GameOver(answer: Int) extends AttemptResult
  final case class Greater(attemptsLeft: Int) extends AttemptResult
  final case class Lower(attemptsLeft: Int) extends AttemptResult

  // TODO: Remove GameNotFound from GameResult
  final case class GameNotFound(gameId: GameId)
}
