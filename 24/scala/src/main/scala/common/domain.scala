package common

import cats.syntax.functor._
import io.circe.{Codec, Decoder, Encoder}
import java.util.UUID

object domain {
  final case class GameId(uuid: UUID) extends AnyVal
  object GameId {
    import cats.effect.Sync
    import effects.GenUUID

    implicit val encoder: Encoder[GameId] = Encoder[UUID].contramap[GameId](gameId => gameId.uuid)
    implicit val decoder: Decoder[GameId] = Decoder[UUID].map(uuid => GameId(uuid))

    def generate[F[_]: Sync](implicit idGen: GenUUID[F]): F[GameId] = idGen.createUUID.map(GameId(_))
  }

  sealed trait GameAction
  final case class NewGame(min: Int, max: Int, attemptCount: Int) extends GameAction
  final case class Guess(gameId: GameId, guess: Int) extends GameAction

  object GameAction {
    import io.circe.generic.auto._
    import io.circe.syntax._

    implicit val encoder: Encoder[GameAction] = Encoder.instance {
      case newGame@NewGame(_, _, _) => newGame.asJson
      case guess@Guess(_, _) => guess.asJson
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
    import io.circe.generic.extras.Configuration
    import io.circe.generic.extras.semiauto.deriveConfiguredCodec

    implicit val genDevConfig: Configuration =
      Configuration.default.withDiscriminator("attemptResult")

    implicit val codec: Codec[AttemptResult] = deriveConfiguredCodec[AttemptResult]
  }

  import cats.Show
  implicit val gameResultShow: Show[AttemptResult] = {
    case YouWon(attemptsUsed, guess) => s"You won. You used $attemptsUsed attempt(s) to guess $guess"
    case GameOver(answer) => s"You lost. Correct answer is $answer"
    case Greater(attemptsLeft) => s"Try to guess lower number. You have $attemptsLeft attempt(s) left"
    case Lower(attemptsLeft) => s"Try to guess greater number. You have $attemptsLeft attempt(s) left"
  }

  final case class YouWon(attemptsUsed: Int, guess: Int) extends AttemptResult
  final case class GameOver(answer: Int) extends AttemptResult
  final case class Greater(attemptsLeft: Int) extends AttemptResult
  final case class Lower(attemptsLeft: Int) extends AttemptResult

  final case class ErrorResponse(error: String)

  final case class GameNotFound(gameId: GameId)
  object GameNotFound {
    import io.circe.generic.auto._

    implicit val encoder: Encoder[GameNotFound] = Encoder[ErrorResponse].contramap { gameNotFound =>
      ErrorResponse(s"There is no game with id: ${gameNotFound.gameId.uuid}")
    }
  }
}
