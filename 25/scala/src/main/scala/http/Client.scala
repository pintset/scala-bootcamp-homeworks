package http

import cats.data.{EitherT, Kleisli}
import cats.implicits.catsSyntaxApplicativeError
import cats.effect.{Concurrent, ConcurrentEffect, ContextShift, IO, Resource}
import cats.implicits.{catsSyntaxApplicativeError, catsSyntaxApply, catsSyntaxFunction1FlatMap, toBifunctorOps}
import org.http4s.Uri
import org.http4s.client.dsl.Http4sClientDsl
import common.domain.{AttemptResult, ErrorResponse, GameAction, GameId, Guess, NewGame}
import org.http4s._
import io.circe.generic.auto._
import org.http4s.circe.CirceEntityCodec._
import cats.syntax.functor._
import cats.syntax.flatMap._
import common.Client
import common.domain.GameId.decoder
import common.domain.AttemptResult.codec
import org.http4s.client.blaze.BlazeClientBuilder
import effects.Console

import java.util.UUID
import scala.concurrent.ExecutionContext
import GameAction.encoder
import io.circe.Decoder

// (Int => F[AttemptResult]) = Client[F]

// Это Guess
// Его можно сделать консольным добавив в конце вывод результата если надо
object Client {
  def apply[F[_]: Concurrent](client: org.http4s.client.Client[F], host: Uri): NewGame => F[Client[F]] = {
    val dsl = new Http4sClientDsl[F] {}
    import dsl._

//    settings =>
//      Concurrent
//        .memoize(Method.POST(settings, host / "start") >>= client.expect[GameId])
//
//        // gameIdF - startRequest. Creating game with gameId
//        .map { gameIdF =>
//          number => gameIdF >>= { gameId => Method.POST(Guess(gameId, number), host / "guess") >>= client.expect[AttemptResult] }
//        }

    def startRequest(settings: NewGame): F[Request[F]] = Method.POST(settings, host / "start")
    def guessRequest(number: Int): GameId => F[Request[F]] = gameId => Method.POST(Guess(gameId, number), host / "guess")

    implicit class ConcurrentOps[A](fa: F[A]) {
      def memoize: F[F[A]] = Concurrent.memoize(fa)
    }

    def expect[R: Decoder](request: Request[F]): F[R] =
      client.run(request).use { response =>
        response.attemptAs[R].leftWiden[Throwable].leftSemiflatMap { failure =>
          response.attemptAs[ErrorResponse]
            .leftWiden[Throwable].fold(_ => failure, er => new Error(er.error))
        }.rethrowT
      }

//    startRequest >=> client.expect[GameId] andThen Concurrent.memoize andThen { gameIdFF =>
//      gameIdFF.map { gameIdF =>
//        number => gameIdF >>= guessRequest(number) >>= client.expect[AttemptResult]
//      }
//    }

//    settings =>
//      Concurrent
//        .memoize(startRequest(settings) >>= client.expect[GameId])
//        .map { gameIdF =>
//          number => gameIdF >>= guessRequest(number) >>= client.expect[AttemptResult] }
//        }

    // Потенциально эту штуку можно будет генерализировать, если передавать ей startRequest/guessRequest
    // Только там и там вызов клиента будет фактически частьюп
    settings =>
      startRequest(settings)
        .flatMap(expect[GameId])
        .memoize
        .map { gameIdF =>
          //number => gameIdF >>= guessRequest(number) >>= client.expect[AttemptResult]
          //number => gameIdF >>= { _ => guessRequest(number)(GameId(UUID.randomUUID())) } >>= expect[AttemptResult]
          number => gameIdF >>= guessRequest(number) >>= expect[AttemptResult]
        }

//    startRequest _
//      .andThenF(client.expect[GameId])
//      .andThen(Concurrent.memoize)
//      .andThen { gameIdFF =>
//        gameIdFF.map { gameIdF =>
//          number => gameIdF >>= guessRequest(number) >>= client.expect[AttemptResult]
//        }
//    }
  }

  def resource[F[_]: Concurrent: ConcurrentEffect](host: Uri): Resource[F, NewGame => F[Client[F]]] =
    BlazeClientBuilder[F](ExecutionContext.global).resource
      // .map { org.http4s.client.middleware.Logger(logHeaders = false, logBody = true) }
      .map { client => Client[F](client, host) }

  //  def resource[F[_] : Concurrent : ConcurrentEffect](host: Uri): Resource[F, NewGame => F[Client[F]]] =
//    BlazeClientBuilder[F](ExecutionContext.global).resource.map { client => Client[F](client, host) }

//  def resource[F[_] : Concurrent : ConcurrentEffect](host: Uri): Resource[F, Kleisli[F, NewGame, Client[F]]] =
//    BlazeClientBuilder[F](ExecutionContext.global).resource.map { client => Kleisli(Client[F](client, host)) }
}