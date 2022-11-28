package server.routes

import cats.effect.Async
import common.domain.{GameAction, GameNotFound, Guess, NewGame}
import fs2.Pipe
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import org.http4s.server.websocket.WebSocketBuilder2
import org.http4s.websocket.WebSocketFrame
import server.GameServer
import io.circe.parser.decode
import cats.syntax.functor._
import cats.syntax.traverse._
import io.circe.syntax.EncoderOps
import common.domain.AttemptResult.codec
import common.domain.GameNotFound.encoder

object Ws {
  def apply[F[_] : Async](gameService: GameServer[F], wsBuilder: WebSocketBuilder2[F]): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F] {}
    import dsl._

    HttpRoutes.of[F] {
      case GET -> Root =>
        val handlerPipe: Pipe[F, WebSocketFrame, WebSocketFrame] =
          _.evalMap {
            case WebSocketFrame.Text(message, _) =>
              decode[GameAction](message)
                .traverse {
                  case Guess(gameId, number) =>
                    gameService.guess(gameId, number).map(_.fold(GameNotFound(gameId).asJson)(_.asJson))

                  case NewGame(min, max, attemptCount) =>
                    gameService.start(min, max, attemptCount).map(_.asJson)
                }
                .map(_.fold(_.getMessage.asJson, identity).toString)
                .map(WebSocketFrame.Text(_))
          }
          
        wsBuilder.build(handlerPipe)
    }
  }
}