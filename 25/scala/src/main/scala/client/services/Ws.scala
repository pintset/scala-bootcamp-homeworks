package client.services

import cats.effect.{Concurrent, ConcurrentEffect, ContextShift, Resource, Sync}
import cats.implicits.catsSyntaxApply
import cats.syntax.flatMap._
import cats.syntax.functor._
import common.domain.{AttemptResult, ErrorResponse, GameAction, GameId, Guess, NewGame}
import effects.Console
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

// (Int => F[AttemptResult]) = Client[F]

// Guess
// Его можно сделать консольным добавив в конце вывод результата если надо
object Ws {
  def apply[F[_]: Concurrent : ContextShift : ConcurrentEffect](client: WSConnectionHighLevel[F], host: Uri) = {
    //    def myDecode[R: Decoder](input: String): Either[Throwable, R] =
    //      parse(input) match {
    //        case Right(json) => Decoder[R].decodeJson(json)
    //        case _ => Left(new Error("The request body was malformed."))
    //      }

    def expect[R: Decoder](gameAction: GameAction): F[R] = {
      Console[F].putStrLn(s"Request: ${gameAction.asJson.toString}") *> client.send(WSFrame.Text(gameAction.asJson.toString)) *>
        client.receiveStream.collectFirst { case WSFrame.Text(s, _) => s }.compile.string
          .flatTap(s => Console[F].putStrLn(s"Response: $s"))
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

  // TODO: Check this thing carefully. Flatmapping on resource. Create resource inside client with start request
  // if possible (so there is no dummy connections
//  def apply[F[_] : Concurrent : ContextShift : ConcurrentEffect](host: Uri): Resource[F, NewGame => F[GameClient[F]]] =
//    Resource.eval(Sync[F].delay(java.net.http.HttpClient.newHttpClient()))
//      .flatMap(JdkWSClient[F](_).connectHighLevel(WSRequest(host)).map(x => GameClient(gameService(x, host))))

    def resource[F[_] : Concurrent : ContextShift : ConcurrentEffect](host: Uri): Resource[F, GameService[F]] =
      Resource.eval(Sync[F].delay(java.net.http.HttpClient.newHttpClient()))
        .flatMap(JdkWSClient[F](_).connectHighLevel(WSRequest(host)).map(x => apply(x, host)))
}