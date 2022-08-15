package effects

import org.scalatest.freespec.AsyncFreeSpec
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.effect.IO
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration.{DurationInt, FiniteDuration}

class SharedStateHomeworkSpec extends AsyncFreeSpec with AsyncIOSpec with Matchers {
  import SharedStateHomework._

  def cacheHasValue(expiresIn: FiniteDuration, checkOnExpirationEvery: FiniteDuration, sleep: FiniteDuration): IO[Boolean] = {
    val io =
      for {
        cache <- Cache.of[IO, Int, Int](expiresIn, checkOnExpirationEvery)
        _ <- cache.put(1, 1)
        _ <- IO.sleep(sleep)
        r <- cache.get(1)
      } yield r

    io.map(_.fold(false)(_ == 1))
  }

  "Value is in cache when exp time and check time are not in sync != 0" in
    cacheHasValue(expiresIn = 4.seconds, checkOnExpirationEvery = 3.seconds, sleep = 5.seconds)
      .asserting(_ shouldBe true)

  "Value removed from cache after expiration" in
    cacheHasValue(expiresIn = 4.seconds, checkOnExpirationEvery = 3.seconds, sleep = 7.seconds)
      .asserting(_ shouldBe false)

  "Expiration is renewed for new value for the same key" in {
    val io =
      for {
        cache <- Cache.of[IO, Int, Int](5.seconds, 3.seconds)
        _ <- cache.put(1, 1)
        _ <- IO.sleep(4.seconds)
        r <- cache.get(1)
        _ <- IO(assert(r == Option(1)))
        _ <- cache.put(1, 1)
        _ <- IO.sleep(4.seconds)
        r <- cache.get(1)
        _ <- IO(assert(r == Option(1)))
        _ <- IO.sleep(3.seconds)
        r <- cache.get(1)
        _ <- IO(assert(r.isEmpty))
      } yield ()

    io.asserting(_ shouldBe ())
  }
}
