import cats.effect.{IO, IOApp, Sync}

import scala.concurrent.ExecutionContext
import org.http4s.implicits.http4sKleisliResponseSyntaxOptionT
import org.http4s.server.blaze.BlazeServerBuilder
import server.GameServer
import ch.qos.logback.classic.LoggerContext
import org.slf4j.LoggerFactory
import cats.syntax.flatMap._
import effects.GenUUID

import scala.util.Random

object ServerApp extends IOApp.Simple {
  def run: IO[Unit] = {
    LoggerFactory.getILoggerFactory.asInstanceOf[LoggerContext].stop()

    implicit val genUUID = GenUUID.make[IO]

    def getNextGuess(min: Int, max: Int): IO[Int] =
      IO(Random.between(min, max + 1))
        .flatTap(guess => Sync[IO].delay(println(s"Guessed number: $guess")))

    GameServer.of[IO](getNextGuess).flatMap { gameServer =>
      BlazeServerBuilder[IO](ExecutionContext.global)
        .bindHttp(port = 9001, host = "localhost")
        .withConnectorPoolSize(1000)
        .withHttpApp(server.Routes(gameServer).orNotFound)
        .serve
        .compile
        .drain
    }
  }
}
