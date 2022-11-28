package client.services

import cats.effect.{Async, Resource, Sync}
import cats.implicits.catsSyntaxApply
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.effect.syntax.concurrent._
import common.domain.{AttemptResult, ErrorResponse, GameAction, GameId, Guess, NewGame}
import io.circe.Decoder
import io.circe.generic.auto._
import io.circe.parser._
import org.http4s.Uri
import io.circe.syntax.EncoderOps
import GameAction.encoder
import GameId.decoder
import AttemptResult.codec
import client.GameService
import org.http4s.client.websocket.{WSConnectionHighLevel, WSFrame, WSRequest}
import org.http4s.jdkhttpclient.JdkWSClient

object Ws {
  def apply[F[_]: Async](client: WSConnectionHighLevel[F]): GameService[F] = {
    def expect[R: Decoder](gameAction: GameAction): F[R] =
      client.send(WSFrame.Text(gameAction.asJson.toString)) *>
        client.receiveStream.collectFirst { case WSFrame.Text(s, _) => s }.compile.string
          .map { input =>
            decode[R](input).left.map { err =>
              decode[ErrorResponse](input).fold(_ => err, re => new Error(re.error))
            }
          }
          .flatMap(Sync[F].fromEither)

    new GameService[F] {
      def start(settings: NewGame): F[GameId] = expect[GameId](settings)
      def guess(gameId: GameId, guess: Int): F[AttemptResult] = expect[AttemptResult](Guess(gameId, guess))
    }
  }

  def resource[F[_] : Async](host: Uri): Resource[F, GameService[F]] =
    Resource.eval(Sync[F].delay(java.net.http.HttpClient.newHttpClient()))
      .flatMap {
        JdkWSClient[F](_).connectHighLevel(WSRequest(host))
          .memoize
          .map { conResource =>
            new GameService[F] {
              def start(settings: NewGame): F[GameId] =
                conResource.use(client => apply[F](client).start(settings))

              def guess(gameId: GameId, guess: Int): F[AttemptResult] =
                conResource.use(client => apply[F](client).guess(gameId, guess))
            }
          }
      }
}