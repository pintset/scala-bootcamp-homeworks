package effects

import cats.Monad
import cats.implicits._
import cats.effect.{Clock, Concurrent, ExitCode, IO, IOApp, Ref, Sync, Temporal}

import scala.concurrent.duration._

/*
 * Please implement a Cache which allows concurrent access.
 *
 * Tip: checking expiration could be represented as some infinite process somewhere in background
 *
 * Tip: you can use following structure to get current time suspended in effect : Clock[F].realTime(MILLISECONDS).flatMap(...)
 *
 * Cached items should have an expiration timestamp after which they are evicted.
 *
 * If we will put a value with the same key then it should renew expiration
 */
object SharedStateHomework extends IOApp {
  trait Cache[F[_], K, V] {
    def get(key: K): F[Option[V]]

    def put(key: K, value: V): F[Unit]
  }

  class RefCache[F[_] : Clock : Monad, K, V](
                                              state: Ref[F, Map[K, (FiniteDuration, V)]],
                                              expiresIn: FiniteDuration
                                            ) extends Cache[F, K, V] {

    def get(key: K): F[Option[V]] =
      state.get.map { items =>
        items.get(key).map { case (_, v) => v }
      }

    def put(key: K, value: V): F[Unit] =
      for {
        now <- Clock[F].realTime
        _ <- state.update { items => items + (key -> (now + expiresIn, value)) }
      } yield ()
  }

  object Cache {
    def of[F[_]: Concurrent, K, V](expiresIn: FiniteDuration, checkOnExpirationsEvery: FiniteDuration)
                                  (implicit timer: Temporal[F], clock: Clock[F]): F[Cache[F, K, V]] = {
      def server(state: Ref[F, Map[K, (FiniteDuration, V)]]): F[Unit] = {
        val iteration =
          for {
            _ <- timer.sleep(checkOnExpirationsEvery)
            now <- clock.realTime
            _ <- state.update { itemsMap => itemsMap.filter { case (_, (expTime, _)) => expTime > now } }
          } yield ()

        iteration.foreverM
      }

      for {
        state <- Ref.of[F, Map[K, (FiniteDuration, V)]](Map.empty)
        _ <- Concurrent[F].start(server(state))
      } yield new RefCache[F, K, V](state, expiresIn)
    }
  }

  override def run(args: List[String]): IO[ExitCode] = {
    implicit class CacheOps[F[_]: Monad: Sync, K, V](cache: Cache[F, K, V]) {
      def getAndPrint(key: K): F[Unit] =
        for {
          value <- cache.get(key)
          _ <- Sync[F].delay(println(s"$key -> $value"))
        } yield ()
    }

    for {
      // Should expire after 10 seconds, but considering checking interval of 4 seconds
      // will expire only after 12 seconds.
      cache <- Cache.of[IO, Int, String](10.seconds, 4.seconds)
      _ <- cache.put(1, "Hello")
      _ <- cache.put(2, "World")

      _ <- cache.getAndPrint(1)
      _ <- cache.getAndPrint(2)

      _ <- IO.sleep(11.seconds)

      _ <- cache.getAndPrint(1)
      _ <- cache.getAndPrint(2)

      _ <- IO.sleep(2.seconds)

      _ <- cache.getAndPrint(1)
      _ <- cache.getAndPrint(2)

    } yield ExitCode.Success
  }
}
