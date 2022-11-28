package server.routes

import common.domain.{GameNotFound, Guess, NewGame}
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import cats.syntax.flatMap._
import org.http4s.circe.CirceEntityCodec._
import server.GameServer
import io.circe.generic.auto._
import common.domain.AttemptResult.codec
import GameNotFound.encoder
import cats.effect.kernel.Concurrent

object Http {
  def apply[F[_] : Concurrent](gameServer: GameServer[F]): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F] {}
    import dsl._

    HttpRoutes.of[F] {
      case req @ POST -> Root / "guess" =>
        req
          .as[Guess]
          .flatMap { guess =>
            gameServer
              .guess(guess.gameId, guess.guess)
              .flatMap(_.fold(NotFound(GameNotFound(guess.gameId)))(Ok(_)))
          }

      case req @ POST -> Root / "start" =>
        req
          .as[NewGame]
          .flatMap { newGame => gameServer.start(newGame.min, newGame.max, newGame.attemptCount) }
          .flatMap(Created(_))
    }
  }
}
