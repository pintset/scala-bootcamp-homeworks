import cats.effect.{Concurrent, IO, IOApp}
import cats.implicits.toSemigroupKOps
// import cats.implicits.toSemigroupKOps
import org.http4s.HttpApp

import scala.concurrent.ExecutionContext
import org.http4s.implicits.http4sKleisliResponseSyntaxOptionT
import org.http4s.server.blaze.BlazeServerBuilder
import server.GameServer

object GuessServer extends IOApp.Simple {
  private def httpApp[F[_]: Concurrent](gameServer: GameServer[F]): HttpApp[F] =
    { http.Routes.routes(gameServer) <+> ws.Routes.routes(gameServer) }.orNotFound

  def run: IO[Unit] =
    GameServer.of[IO].flatMap { gameServer =>
      BlazeServerBuilder[IO](ExecutionContext.global)
        .bindHttp(port = 9001, host = "localhost")
        .withConnectorPoolSize(1000)
        .withHttpApp(httpApp(gameServer))
        .serve
        .compile
        .drain
    }
}
