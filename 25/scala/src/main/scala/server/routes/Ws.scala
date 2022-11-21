package server.routes

import cats.effect.Concurrent
import common.domain.{GameAction, GameNotFound, Guess, NewGame}
import effects.Console
import fs2.Pipe
import fs2.concurrent.Queue
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import org.http4s.server.websocket.WebSocketBuilder
import org.http4s.websocket.WebSocketFrame
import server.GameServer
import io.circe.parser.decode
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.syntax.traverse._
import io.circe.syntax.EncoderOps

import common.domain.GameId.encoder
import common.domain.AttemptResult.codec
import common.domain.GameNotFound.encoder

object Ws {
  def apply[F[_] : Concurrent](gameService: GameServer[F]): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F] {}
    import dsl._

    HttpRoutes.of[F] {
      // TODO: "game" in path here. Maybe can be removed
      // case GET -> Root / "game" =>
      case GET -> Root =>
        val gamePipe: Pipe[F, WebSocketFrame, WebSocketFrame] =
          _.evalMap {
            case WebSocketFrame.Text(message, _) =>
              Console[F].putStrLn(s"Request: ${decode[GameAction](message)}") >>
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

        Queue.unbounded[F, WebSocketFrame] >>= { queue =>
          WebSocketBuilder[F].build(
            receive = queue.enqueue,
            send = queue.dequeue.through(gamePipe)
          )
        }
    }
  }
}
