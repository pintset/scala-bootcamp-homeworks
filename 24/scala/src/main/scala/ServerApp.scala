import cats.effect.{IO, IOApp}
import com.comcast.ip4s.IpLiteralSyntax
import effects.GenUUID
import server.routes
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits.http4sKleisliResponseSyntaxOptionT
import server.GameServer
import cats.syntax.semigroupk._

import scala.util.Random

object ServerApp extends IOApp.Simple {
  def run: IO[Unit] = {
    implicit val genUUID = GenUUID.make[IO]

    def getNextGuess(min: Int, max: Int): IO[Int] =
      IO(Random.between(min, max + 1))
        .flatTap(guess => IO.println(s"Guessed number: $guess"))

    GameServer.of[IO](getNextGuess).flatMap { gameServer =>
      EmberServerBuilder
        .default[IO]
        .withHost(host"localhost")
        .withPort(port"9001")
        .withMaxConnections(1000)
        .withHttpWebSocketApp(builder => (routes.Http(gameServer) <+> routes.Ws(gameServer, builder)).orNotFound)
        .build
        .useForever
    }
  }
}