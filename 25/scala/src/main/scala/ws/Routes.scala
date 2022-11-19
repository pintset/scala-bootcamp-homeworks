package ws

import cats.effect.{Concurrent, Sync}
import common.domain.{GameAction, GameNotFound, Guess, NewGame}
import fs2.Pipe
import fs2.concurrent.Queue
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import org.http4s.server.websocket.WebSocketBuilder
import org.http4s.websocket.WebSocketFrame
import server.GameServer
import io.circe.parser._
import cats.syntax.flatMap._
import cats.syntax.functor._
import io.circe.syntax.EncoderOps
import effects.Console
import io.circe.generic.auto._
import org.http4s.circe.CirceEntityCodec._
import GameAction.decoder
import GameNotFound.encoder

object Routes {
  def apply[F[_]: Concurrent](gameService: GameServer[F]): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F] {}
    import dsl._

    HttpRoutes.of[F] {
      // TODO: "game" in path here. Maybe can be removed
      // case GET -> Root / "game" =>
      case GET -> Root =>
        val gamePipe: Pipe[F, WebSocketFrame, WebSocketFrame] =
          _.evalMap {
            case WebSocketFrame.Text(message, _) =>
              for {
                _ <- Console[F].putStrLn(s"Request: ${decode[GameAction](message)}")
                gameAction <- Sync[F].fromEither(decode[GameAction](message))

                response <- gameAction match {
                  case Guess(gameId, number) =>
                    gameService.guess(gameId, number).map(_.fold(GameNotFound(gameId).asJson)(_.asJson))

                  case NewGame(min, max, attemptCount) =>
                    gameService.start(min, max, attemptCount).map(_.asJson)
                }
              } yield WebSocketFrame.Text(response.toString)
            }

        for {
          queue <- Queue.unbounded[F, WebSocketFrame]
          response <- WebSocketBuilder[F].build(
            receive = queue.enqueue,
            send = queue.dequeue.through(gamePipe)
          )
        } yield response
    }
  }
}
