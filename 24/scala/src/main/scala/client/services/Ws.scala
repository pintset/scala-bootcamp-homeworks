package client.services

import cats.effect.{Concurrent, ConcurrentEffect, ContextShift, Resource, Sync}
import cats.implicits.catsSyntaxApply
import cats.syntax.flatMap._
import cats.syntax.functor._
import common.domain.{AttemptResult, ErrorResponse, GameAction, GameId, Guess, NewGame}
import io.circe.Decoder
import io.circe.generic.auto._
import io.circe.parser._
import org.http4s.Uri
import org.http4s.client.jdkhttpclient.{JdkWSClient, WSConnectionHighLevel, WSFrame, WSRequest}
import io.circe.syntax.EncoderOps
import GameAction.encoder
import GameId.decoder
import AttemptResult.codec
import client.GameService

object Ws {
  def apply[F[_]: Concurrent : ContextShift : ConcurrentEffect](client: WSConnectionHighLevel[F]): GameService[F] = {
    def expect[R: Decoder](gameAction: GameAction): F[R] = {
      // effects.Console[F].putStrLn(s"Request: ${gameAction.asJson.toString}") *>
      client.send(WSFrame.Text(gameAction.asJson.toString)) *>
        client.receiveStream.collectFirst { case WSFrame.Text(s, _) => s }.compile.string
          // .flatTap(s => effects.Console[F].putStrLn(s"Response: $s"))
          .map { input =>
            decode[R](input).left.map { err =>
              decode[ErrorResponse](input).fold(_ => err, re => new Error(re.error))
            }
          }
          .flatMap(Sync[F].fromEither)
    }

    new GameService[F] {
      def start(settings: NewGame): F[GameId] = expect[GameId](settings)
      def guess(gameId: GameId, guess: Int): F[AttemptResult] = expect[AttemptResult](Guess(gameId, guess))
    }
  }

  def resource[F[_] : Concurrent : ContextShift : ConcurrentEffect](host: Uri): Resource[F, GameService[F]] =
    Resource.eval(Sync[F].delay(java.net.http.HttpClient.newHttpClient()))
      .flatMap(JdkWSClient[F](_).connectHighLevel(WSRequest(host)).map(apply[F]))
}