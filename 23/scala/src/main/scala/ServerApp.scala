import cats.effect.{IO, IOApp}
import scala.concurrent.ExecutionContext
import org.http4s.implicits.http4sKleisliResponseSyntaxOptionT
import org.http4s.server.blaze.BlazeServerBuilder
import server.GameServer

import ch.qos.logback.classic.LoggerContext
import org.slf4j.LoggerFactory

object ServerApp extends IOApp.Simple {
  def run: IO[Unit] = {
    LoggerFactory.getILoggerFactory.asInstanceOf[LoggerContext].stop()

    GameServer.of[IO].flatMap { gameServer =>
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
