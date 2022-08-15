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
    cacheHasValue(expiresIn = 100.milliseconds, checkOnExpirationEvery = 70.milliseconds, sleep = 120.milliseconds)
      .asserting(_ shouldBe true)

  "Value removed from cache after expiration" in
    cacheHasValue(expiresIn = 100.milliseconds, checkOnExpirationEvery = 50.milliseconds, sleep = 120.milliseconds)
      .asserting(_ shouldBe false)

  "Expiration is renewed for new value for the same key" in {
    val io =
      for {
        cache <- Cache.of[IO, Int, Int](100.milliseconds, 70.milliseconds)
        _ <- cache.put(1, 1)
        _ <- IO.sleep(120.milliseconds)
        r <- cache.get(1)
        _ <- IO(assert(r == Option(1)))
        _ <- cache.put(1, 1)
        _ <- IO.sleep(80.milliseconds)
        r <- cache.get(1)
        _ <- IO(assert(r == Option(1)))
        _ <- IO.sleep(50.milliseconds)
        r <- cache.get(1)
        _ <- IO(assert(r.isEmpty))
      } yield ()

    io.asserting(_ shouldBe ())
  }
}
