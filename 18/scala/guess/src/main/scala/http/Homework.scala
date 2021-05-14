package http

import java.util.UUID
import cats.effect.{ExitCode, IO, IOApp}
import org.http4s._
import org.http4s.implicits._

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
}

object GuessServer extends IOApp {
  import scala.util.Random
  import io.circe.generic.auto._
  import cats.data.Kleisli
  import cats.effect.concurrent.Ref
  import org.http4s.circe.CirceEntityCodec._
  import org.http4s.dsl.io._
  import org.http4s.server.blaze.BlazeServerBuilder
  import http.Protocol._

  final case class Game(number: Int, attemptsLeft: Int)

  private def helloRoutes(ref: Ref[IO, Map[UUID, Game]]): HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req @ POST -> Root / "guess" =>
      for {
        guess <- req.as[Guess]
        gameOpt <- ref.modifyMaybe { games =>
          games.get(guess.id).map { game =>
            val newGame = game.copy(attemptsLeft = game.attemptsLeft - 1)
            if (newGame.attemptsLeft == 0 || guess.number == newGame.number)
              (games - guess.id, newGame)
            else
              (games + (guess.id -> newGame), newGame)
          }
        }
        response <- gameOpt.map { game =>
          val result: GameResult =
            if (guess.number == game.number) YouWon
            else if (game.attemptsLeft == 0) GameOver
            else if (guess.number > game.number) Greater
            else Lower
          Ok(result)
        }.getOrElse(Ok(GameNotFound.asInstanceOf[GameResult]))
      } yield response

    case req @ POST -> Root / "start" => Created(
      for {
        newGame <- req.as[NewGame]
        id = UUID.randomUUID()
        _ <- ref.update(_ + (id -> Game(Random.between(newGame.min, newGame.max), 5)))
        _ <- ref.get.map { map =>
          map
        }
      } yield id
    )
  }

  private def httpApp(ref: Ref[IO, Map[UUID, Game]]): Kleisli[IO, Request[IO], Response[IO]] = {
    helloRoutes(ref)
  }.orNotFound

  override def run(args: List[String]): IO[ExitCode] =
    Ref.of[IO, Map[UUID, Game]](Map.empty).flatMap { ref =>
      BlazeServerBuilder[IO](ExecutionContext.global)
        .bindHttp(port = 9001, host = "localhost")
        .withConnectorPoolSize(1000)
        .withHttpApp(httpApp(ref))
        .serve
        .compile
        .drain
        .as(ExitCode.Success)
    }
}

object GuessClient extends IOApp {
  import io.circe.generic.auto._
  import cats.syntax.all._

  import org.http4s.circe.CirceEntityCodec._
  import org.http4s.client.blaze.BlazeClientBuilder
  import org.http4s.client.Client
  import org.http4s.client.dsl.io._
  import http.Protocol._

  private val uri = uri"http://localhost:9001"

  private def printLine(string: String = ""): IO[Unit] = IO(println(string))

  override def run(args: List[String]): IO[ExitCode] = {
    val min = 10
    val max = 100

    def guess(client: Client[IO], id: UUID, min: Int, max: Int): IO[String] = {
      val number = min + (max - min) / 2
      client.expect[GameResult](Method.POST(Guess(id, number), uri / "guess")).flatMap {
        case YouWon => IO(s"You won. Number is: ${number.toString}")
        case Greater => guess(client, id, min, number)
        case Lower => guess(client, id, number, max)
        case GameOver => IO("Game over")
        case GameNotFound => IO("Game not found")
      }
    }

    BlazeClientBuilder[IO](ExecutionContext.global).resource.use { client =>
      for {
        _ <- printLine(string = "Start game")
        id <- client.expect[UUID](Method.POST(NewGame(min, max), uri / "start"))
        _ <- printLine(s"Game id: ${id.toString}")
        _ <- guess(client, id, min, max) >>= printLine
        _ <- printLine()
      } yield ()
    }.as(ExitCode.Success)

//    def gameIO(client: Client[IO]): IO[String] =
//      for {
//        id <- client.expect[UUID](Method.POST(NewGame(min, max), uri / "start"))
//        response <- guess(client, id, min, max)
//      } yield response
//
//    import java.time.LocalTime
//    import java.time.format.DateTimeFormatter
//
//    val formatter = DateTimeFormatter.ofPattern("HH:mm:ss.SSS")
//
//    def measured[A](io: IO[A]): IO[A] = for {
//      startTime <- IO(LocalTime.now)
//      results <- io
//      runTime <- IO(LocalTime.now.minusNanos(startTime.toNanoOfDay))
//      _ <- printLine(s"Run time: ${formatter.format(runTime)}")
//    } yield results
//
//    def loop[A](n: Int, io: IO[A]): IO[List[A]]=
//      (1 to n).toList.traverse(_ => io)
//
//    BlazeClientBuilder[IO](ExecutionContext.global).withMaxWaitQueueLimit(10000).resource.use { client =>
//      for {
//        _ <- loop(100, measured((1 to 1000).toList.parTraverse(_ => gameIO(client))))
//        // _ <- gameIO(client) >>= printLine
//        // _ <- IO(results.foreach(println))
//        _ <- printLine()
//      } yield ()
//    }.as(ExitCode.Success)
  }
}
