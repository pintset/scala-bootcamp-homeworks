package http

import cats.data.Kleisli
import cats.effect.{Concurrent, ConcurrentEffect, ContextShift, IO, Resource}
import org.http4s.Uri
import org.http4s.client.dsl.Http4sClientDsl
import common.domain.{AttemptResult, GameId, Guess, NewGame}
import org.http4s._
import io.circe.generic.auto._
import org.http4s.circe.CirceEntityCodec._
import cats.syntax.functor._
import cats.syntax.flatMap._
import common.Client
import common.domain.GameId.decoder
import common.domain.AttemptResult.codec
import org.http4s.client.blaze.BlazeClientBuilder

import scala.concurrent.ExecutionContext

// (Int => F[AttemptResult]) = Client[F]

// Это Guess
// Его можно сделать консольным добавив в конце вывод результата если надо
object Client {
  def apply[F[_]: Concurrent](client: org.http4s.client.Client[F], host: Uri): NewGame => F[Client[F]] = {
    val dsl = new Http4sClientDsl[F] {}
    import dsl._

    settings =>
      Concurrent
        .memoize(Method.POST(settings, host / "start") >>= client.expect[GameId])

        // gameIdF - startRequest. Creating game with gameId
        .map { gameIdF =>
          (number: Int) => gameIdF >>= { gameId => Method.POST(Guess(gameId, number), host / "guess") >>= client.expect[AttemptResult] }
        }
  }

  def resource[F[_]: Concurrent: ConcurrentEffect](host: Uri): Resource[F, Kleisli[F, NewGame, Client[F]]] =
    BlazeClientBuilder[F](ExecutionContext.global).resource.map { client => Kleisli(Client[F](client, host)) }
}