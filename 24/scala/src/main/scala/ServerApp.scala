import cats.effect.{Async, IO, IOApp, Sync}
import com.comcast.ip4s.IpLiteralSyntax
import effects.GenUUID
import server.{GameServer, routes}
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits.http4sKleisliResponseSyntaxOptionT
import cats.syntax.semigroupk._
import cats.syntax.flatMap._
import cats.syntax.functor._
import io.circe.{Decoder, Encoder}
import org.http4s.HttpApp
import org.http4s.server.websocket.WebSocketBuilder2

import scala.util.Random

object ServerApp extends IOApp.Simple {
  def httpApp[F[_]: Async, A: Encoder: Decoder](gameServer: GameServer[F, A])(builder: WebSocketBuilder2[F]): HttpApp[F] =
    (routes.Http(gameServer) <+> routes.Ws(gameServer, builder)).orNotFound

  def getNextGuess[F[_] : Sync]: F[String] = {
    val words = Array(
      "вихрь",
      "почта",
      "тембр"
    )

    Sync[F].delay(Random.between(0, words.length))
      .map(words)
      .flatTap(guess => Sync[F].delay(println(s"Guess: $guess")))
  }

  def program[F[_] : Async, A: Encoder: Decoder](gameServer: GameServer[F, A]): F[Nothing] =
    EmberServerBuilder
      .default[F]
      .withHost(host"localhost")
      .withPort(port"9001")
      .withMaxConnections(1000)
      .withHttpWebSocketApp(httpApp(gameServer))
      .build
      .useForever

  def run: IO[Unit] = {
    implicit val genUUID = GenUUID.make[IO]
    implicit val gameResult = common.domain.gameResult
    GameServer.of[IO, String](getNextGuess[IO]) >>= program[IO, String]
  }
}