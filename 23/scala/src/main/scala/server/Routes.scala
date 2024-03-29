package server

import cats.effect.Sync
import common.domain.{GameNotFound, Guess, NewGame}
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import io.circe.generic.auto._
import cats.syntax.flatMap._
import org.http4s.circe.CirceEntityCodec._
import common.domain.GameId.encoder
import common.domain.AttemptResult.codec
import GameNotFound.encoder

object Routes {
  def apply[F[_] : Sync](gameService: GameServer[F]): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F] {}
    import dsl._

    HttpRoutes.of[F] {
      case req @ POST -> Root / "guess" =>
        req
          .as[Guess]
          .flatMap { guess =>
            gameService
              .guess(guess.gameId, guess.guess)
              .flatMap(_.fold(NotFound(GameNotFound(guess.gameId)))(Ok(_)))
          }

      case req @ POST -> Root / "start" =>
        req
          .as[NewGame]
          .flatMap { newGame => gameService.start(newGame.min, newGame.max, newGame.attemptCount) }
          .flatMap(Created(_))
    }
  }
}
