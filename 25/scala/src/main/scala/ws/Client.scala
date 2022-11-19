package ws

import cats.effect.{Concurrent, ConcurrentEffect, ContextShift, IO, Resource, Sync}
import cats.implicits.{catsSyntaxApply, catsSyntaxFunction1FlatMap}
import org.http4s.Uri
import common.domain.{AttemptResult, ErrorResponse, GameAction, GameId, Guess, NewGame}
import cats.syntax.functor._
import cats.syntax.flatMap._
import common.Client
import io.circe.syntax.EncoderOps
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.client.jdkhttpclient.{JdkWSClient, WSClient, WSConnectionHighLevel, WSFrame, WSRequest}
import io.circe.parser._
import io.circe.generic.auto._

import scala.concurrent.ExecutionContext
import effects.Console
import io.circe.Decoder
import GameAction.encoder

import java.util.UUID

// (Int => F[AttemptResult]) = Client[F]

// Это Guess
// Его можно сделать консольным добавив в конце вывод результата если надо
object Client {
  def apply[F[_]: Concurrent : ContextShift : ConcurrentEffect](client: WSConnectionHighLevel[F], host: Uri): NewGame => F[Client[F]] = {
    def expect[R: Decoder](gameAction: GameAction): F[R] = {
      Console[F].putStrLn(s"Request: ${gameAction.asJson.toString}") *> client.send(WSFrame.Text(gameAction.asJson.toString)) *>
        client.receiveStream.collectFirst { case WSFrame.Text(s, _) => s }.compile.string
          .flatTap(s => Console[F].putStrLn(s"Response: $s"))
          .map { x =>
            decode[R](x) match {
              case err @ Left(error) =>
                decode[ErrorResponse](x) match {
                  case Left(_) => err
                  case Right(responseError) => Left(new Error(responseError.error))
                }
              case Right(y) => Right(y)
            }
          }
          .flatMap(Sync[F].fromEither)
    }

    settings =>
      Concurrent
        .memoize {
          expect[GameId](settings)
        }

        // gameIdF - startRequest. Creating game with gameId
        .map { gameIdF =>
          number => gameIdF >>= { gameId => expect[AttemptResult](Guess(gameId, number)) }
//          number => gameIdF >>= {
//            gameId => expect[AttemptResult](Guess(GameId(UUID.randomUUID()), number))
//          }
        }
  }

  // TODO: Check this thing carefully. Flatmapping on resource. Create resource inside client with start request
  // if possible (so there is no dummy connections
  def resource[F[_] : Concurrent : ContextShift : ConcurrentEffect](host: Uri): Resource[F, NewGame => F[Client[F]]] =
    Resource.eval(Sync[F].delay(java.net.http.HttpClient.newHttpClient()))
      .flatMap(JdkWSClient[F](_).connectHighLevel(WSRequest(host)).map(x => apply(x, host)))
}