package http

import cats.effect.Concurrent
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

object Client {
  def apply[F[_]: Concurrent](client: org.http4s.client.Client[F], host: Uri, settings: NewGame): F[Client[F]] = {
    val dsl = new Http4sClientDsl[F] {}
    import dsl._

    Concurrent
      .memoize(Method.POST(settings, host / "start") >>= client.expect[GameId])

      // gameIdF - startRequest. Creating game with gameId
      .map { gameIdF =>
        (number: Int) => gameIdF >>= { gameId => Method.POST(Guess(gameId, number), host / "guess") >>= client.expect[AttemptResult] }
      }
  }
}