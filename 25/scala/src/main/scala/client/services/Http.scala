package client.services

import cats.effect.{Concurrent, ConcurrentEffect, Resource}
import common.domain.{AttemptResult, ErrorResponse, GameId, Guess, NewGame}
import io.circe.Decoder
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.client.dsl.Http4sClientDsl
import org.http4s.{Method, Request, Uri}
import cats.syntax.flatMap._
import cats.syntax.bifunctor._
import io.circe.generic.auto._
import org.http4s.circe.CirceEntityCodec._
import GameId.decoder
import AttemptResult.codec
import client.GameService

import scala.concurrent.ExecutionContext

object Http {
  def apply[F[_] : Concurrent](client: org.http4s.client.Client[F], host: Uri): GameService[F] = {
    val dsl = new Http4sClientDsl[F] {}
    import dsl._

    def expect[R: Decoder](request: Request[F]): F[R] =
      client.run(request).use { response =>
        response.attemptAs[R].leftWiden[Throwable].leftSemiflatMap { failure =>
          response.attemptAs[ErrorResponse]
            .leftWiden[Throwable].fold(_ => failure, er => new Error(er.error))
        }.rethrowT
      }

    new GameService[F] {
      def start(settings: NewGame): F[GameId] =
        Method.POST(settings, host / "start") >>= expect[GameId]

      def guess(gameId: GameId, guess: Int): F[AttemptResult] =
        Method.POST(Guess(gameId, guess), host / "guess") >>= expect[AttemptResult]
    }
  }

  def resource[F[_] : Concurrent : ConcurrentEffect](host: Uri): Resource[F, GameService[F]] =
    BlazeClientBuilder[F](ExecutionContext.global).resource
      // .map { org.http4s.client.middleware.Logger(logHeaders = false, logBody = true) }
      .map { client => apply(client, host) }
}