package http

import java.util.UUID
import cats.effect.{ExitCode, IO, IOApp}
import fs2.Pipe
import fs2.concurrent.Queue
import http.Protocol.Topic
import org.http4s.client.dsl.Http4sClientDsl
import io.circe.Encoder
import io.circe.Decoder
import org.http4s._
import org.http4s.client.jdkhttpclient.WSConnectionHighLevel
import org.http4s.implicits._
import org.http4s.server.websocket.WebSocketBuilder
import org.http4s.websocket.WebSocketFrame

import scala.concurrent.ExecutionContext

object Protocol {
  final case class NewGame(min: Int, max: Int)
  final case class Guess(id: UUID, number: Int)

  sealed trait GameResult
  case object YouWon extends GameResult
  case object GameOver extends GameResult
  case object Greater extends GameResult
  case object Lower extends GameResult
  case object GameNotFound extends GameResult

  sealed trait Topic
  case object Start extends Topic
  case object Guess extends Topic

  final case class Msg[A](topic: Topic, payload: A)
}

trait GameService[F[_]] {
  def start(min: Int, max: Int): F[UUID]
  def guess(id: UUID, number: Int): F[Protocol.GameResult]
}

object GuessServer extends IOApp {
  import scala.util.Random
  import io.circe.generic.auto._
  import io.circe.parser._
  import io.circe.syntax._
  import cats.data.Kleisli
  import cats.effect.concurrent.Ref
  import cats.syntax.all._
  import org.http4s.circe.CirceEntityCodec._
  import org.http4s.dsl.io._
  import org.http4s.server.blaze.BlazeServerBuilder
  import http.Protocol._

  final case class Game(number: Int, attemptsLeft: Int)

  object GameServer {
    import cats.Monad

    def apply[F[_]: Monad](ref: Ref[F, Map[UUID, Game]]): GameService[F] = new GameService[F] {
      override def start(min: Int, max: Int): F[UUID] =
        for {
          id <- Monad[F].pure(UUID.randomUUID())
          _ <- ref.update(_ + (id -> Game(Random.between(min, max), 5)))
        } yield id

      override def guess(id: UUID, number: Int): F[GameResult] =
        for {
          gameOpt <- ref.modifyMaybe { games =>
            games.get(id).map { game =>
              val newGame = game.copy(attemptsLeft = game.attemptsLeft - 1)
              if (newGame.attemptsLeft == 0 || number == newGame.number)
                (games - id, newGame)
              else
                (games + (id -> newGame), newGame)
            }
          }
          result <- gameOpt.map { game =>
            val result: GameResult =
              if (number == game.number) YouWon
              else if (game.attemptsLeft == 0) GameOver
              else if (number > game.number) Greater
              else Lower
            Monad[F].pure(result)
          }.getOrElse(Monad[F].pure(GameNotFound.asInstanceOf[GameResult]))
        } yield result
    }
  }

  private def httpRoutes(gameService: GameService[IO]): HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req @ POST -> Root / "guess" => Ok(
      for {
        guess <- req.as[Guess]
        result <- gameService.guess(guess.id, guess.number)
      } yield result
    )

    case req @ POST -> Root / "start" => Created(
      for {
        newGame <- req.as[NewGame]
        id <- gameService.start(newGame.min, newGame.max)
      } yield id
    )
  }

  private def socketRoute(gameService: GameService[IO]): HttpRoutes[IO] = HttpRoutes.of[IO] {
    case GET -> Root / "game" =>
      def reqToRes[A: Decoder, B: Encoder](s: String, f: A => IO[B]): IO[String] =
        IO.fromEither(decode[Msg[A]](s)).map(_.payload).flatMap(f).map(_.asJson.toString)

      val gamePipe: Pipe[IO, WebSocketFrame, WebSocketFrame] =
        _.evalMap {
          case WebSocketFrame.Text(message, _) => for {
            req <- IO.fromEither(parse(message))
            topic <- IO.fromEither(req.hcursor.get[Topic]("topic"))
            res <- topic match {
              case Guess => reqToRes[Guess, GameResult](message, guess => gameService.guess(guess.id, guess.number))
              case Start => reqToRes[NewGame, UUID](message, newGame => gameService.start(newGame.min, newGame.max))
            }
          } yield WebSocketFrame.Text(res)
        }

      for {
        queue <- Queue.unbounded[IO, WebSocketFrame]
        response <- WebSocketBuilder[IO].build(
          receive = queue.enqueue,
          send = queue.dequeue.through(gamePipe)
        )
      } yield response
  }

  private def httpApp(gameService: GameService[IO]): Kleisli[IO, Request[IO], Response[IO]] = {
    httpRoutes(gameService) <+> socketRoute(gameService)
  }.orNotFound

  override def run(args: List[String]): IO[ExitCode] =
    Ref.of[IO, Map[UUID, Game]](Map.empty).flatMap { ref =>
      BlazeServerBuilder[IO](ExecutionContext.global)
        .bindHttp(port = 9001, host = "localhost")
        .withConnectorPoolSize(1000)
        .withHttpApp(httpApp(GameServer(ref)))
        .serve
        .compile
        .drain
        .as(ExitCode.Success)
    }
}

trait WebClient[F[_]] {
  def expect[A: Encoder, B: Decoder](body: A, topic: Topic): F[B]
}

object WsClient {
  import cats.effect.Sync
  import org.http4s.client.jdkhttpclient.WSFrame
  import Protocol._
  import io.circe.generic.auto._
  import io.circe.parser._
  import io.circe.syntax._
  import cats.syntax.all._

  def apply[F[_] : Sync](client: WSConnectionHighLevel[F]): WebClient[F] = new WebClient[F] {
    override def expect[A: Encoder, B: Decoder](body: A, topic: Topic): F[B] =
      client.send(WSFrame.Text(Msg(topic, body).asJson.toString)) *>
        client.receiveStream.collectFirst { case WSFrame.Text(s, _) => s }.compile.string
          .map(decode[B](_)).flatMap(Sync[F].fromEither)
  }
}

object HttpClient {
  import cats.effect.Sync

  import org.http4s.circe.CirceEntityCodec._
  import org.http4s.client.Client

  def apply[F[_] : Sync](client: Client[F], uri: Uri): WebClient[F] = {
    val dsl = new Http4sClientDsl[F] { }
    import dsl._

    new WebClient[F] {
      override def expect[A: Encoder, B: Decoder](body: A, topic: Topic): F[B] =
        client.expect[B](Method.POST(body, uri / topic.toString.toLowerCase))
    }
  }
}

trait GameClient[F[_]] {
  def start(min: Int, max: Int): F[UUID]
  def guess(id: UUID, min: Int, max: Int): F[String]
}

object GameClient {
  import cats.effect.Sync
  import cats.syntax.all._
  import io.circe.generic.auto._
  import Protocol._

  def apply[F[_] : Sync](client: WebClient[F]): GameClient[F] = new GameClient[F] {
    override def start(min: Int, max: Int): F[UUID] =
      client.expect(NewGame(min, max), Start)

    override def guess(id: UUID, min: Int, max: Int): F[String] = {
      val number = min + (max - min) / 2
      client.expect[Guess, GameResult](Guess(id, number), Guess)
        .flatMap {
          case YouWon => Sync[F].pure(s"You won. Number is: ${number.toString}")
          case Greater => guess(id, min, number)
          case Lower => guess(id, number, max)
          case GameOver => Sync[F].pure("Game over")
          case GameNotFound => Sync[F].pure("Game not found")
        }
    }
  }
}

object GuessClientWs extends IOApp {
  import cats.effect.{ExitCode, IO, Resource}
  import cats.syntax.all._
  import org.http4s.client.jdkhttpclient.{JdkWSClient, WSRequest}

  private val uri = uri"ws://localhost:9001/game"

  private def printLine(string: String = ""): IO[Unit] = IO(println(string))

  override def run(args: List[String]): IO[ExitCode] = {
    val min = 10
    val max = 100

    val clientResource = Resource.eval(IO(java.net.http.HttpClient.newHttpClient()))
      .flatMap(JdkWSClient[IO](_).connectHighLevel(WSRequest(uri)))

    clientResource.use { client =>
      val gameClient = GameClient(WsClient(client))
      for {
        _ <- printLine(string = "Start game")
        id <- gameClient.start(min, max)
        _ <- printLine(s"Game id: ${id.toString}")
        _ <- gameClient.guess(id, min, max) >>= printLine
        _ <- printLine()
      } yield ExitCode.Success
    }
  }
}

object GuessClientHttp extends IOApp {
  import cats.syntax.all._
  import org.http4s.client.blaze.BlazeClientBuilder

  private val uri = uri"http://localhost:9001"

  private def printLine(string: String = ""): IO[Unit] = IO(println(string))

  override def run(args: List[String]): IO[ExitCode] = {
    val min = 10
    val max = 100

    BlazeClientBuilder[IO](ExecutionContext.global).resource.use { client =>
      val gameClient = GameClient(HttpClient(client, uri))
      for {
        _ <- printLine(string = "Start game")
        id <- gameClient.start(min, max)
        _ <- printLine(s"Game id: ${id.toString}")
        _ <- gameClient.guess(id, min, max) >>= printLine
        _ <- printLine()
      } yield ()
    }.as(ExitCode.Success)
  }
}
