package crud

import java.time.{LocalDate, Year}
import java.util.UUID
import cats.effect.IOApp
import org.http4s._
import org.http4s.implicits._

import java.time.format.DateTimeFormatter
import scala.concurrent.ExecutionContext

object Protocol {
  final case class BookDto(title: String, authorId: UUID, genreId: UUID, year: Year)
  final case class AuthorDto(name: String, birthday: LocalDate)
  final case class GenreDto(name: String)
}

object Matchers {
  import org.http4s.dsl.io.OptionalQueryParamDecoderMatcher

  implicit val birthdayParamDecoder: QueryParamDecoder[LocalDate] = QueryParamDecoder.localDateQueryParamDecoder(DateTimeFormatter.ISO_LOCAL_DATE)
  implicit val yearQueryParamDecoder: QueryParamDecoder[Year] = QueryParamDecoder[Int].map(Year.of)

  object NameParamMatcher extends OptionalQueryParamDecoderMatcher[String]("name")
  object BirthdayParamMatcher extends OptionalQueryParamDecoderMatcher[LocalDate]("birthday")

  object TitleParamMatcher extends OptionalQueryParamDecoderMatcher[String]("title")
  object YearParamMatcher extends OptionalQueryParamDecoderMatcher[Year]("year")
  object AuthorParamMatcher extends OptionalQueryParamDecoderMatcher[String]("author")
  object GenreParamMatcher extends OptionalQueryParamDecoderMatcher[String]("genre")
}

object BookServer extends IOApp {
  import io.circe.generic.auto._

  import org.http4s.circe.CirceEntityCodec._
  import org.http4s.dsl.io._
  import org.http4s.server.blaze.BlazeServerBuilder

  import cats.data.Kleisli
  import cats.effect._
  import cats.implicits._
  import doobie._
  import doobie.implicits._

  import Data._
  import Matchers._
  import Protocol._

  private def optResponse[A](opt: Option[A])(implicit encoder: EntityEncoder[IO, A]):IO[Response[IO]] =
    opt.map(Ok(_)).getOrElse(NotFound())

  private def bookRoutes(bookRepo: BookRepository): HttpRoutes[IO] = HttpRoutes.of[IO] {
    case GET -> Root / "books" :? TitleParamMatcher(title) +& YearParamMatcher(year) +& AuthorParamMatcher(author) +& GenreParamMatcher(genre) =>
      Ok(bookRepo.search(title, year, author, genre))

    case GET -> Root / "books" / UUIDVar(id) =>
      bookRepo.getById(id).flatMap(optResponse(_))

    case req @ POST -> Root / "books" =>
      req.as[BookDto].flatMap(dto => Created(bookRepo.insert(dto.title, dto.authorId, dto.genreId, dto.year)))

    case req @ PUT -> Root / "books" =>
      req.as[Book]
        .flatMap(dto => bookRepo.update(dto.id, dto.title, dto.authorId, dto.genreId, dto.year))
        .flatMap(optResponse(_))

    case DELETE -> Root / "books" / UUIDVar(id) =>
      bookRepo.delete(id).flatMap(optResponse(_))
  }

  private def authorRoutes(authorRepo: AuthorRepository): HttpRoutes[IO] = HttpRoutes.of[IO] {
    case GET -> Root / "authors" :? NameParamMatcher(name) +& BirthdayParamMatcher(birthday) =>
      Ok(authorRepo.search(name, birthday))

    case GET -> Root / "authors" / UUIDVar(id) =>
      authorRepo.getById(id).flatMap(optResponse(_))

    case req @ POST -> Root / "authors" =>
      req.as[AuthorDto].flatMap(dto => Created(authorRepo.insert(dto.name, dto.birthday)))

    case req @ PUT -> Root / "authors" =>
      req.as[Author]
        .flatMap(dto => authorRepo.update(dto.id, dto.name, dto.birthday))
        .flatMap(optResponse(_))

    case DELETE -> Root / "authors" / UUIDVar(id) =>
      authorRepo.delete(id).flatMap(optResponse(_))
  }

  private def genreRoutes(genreRepo: GenreRepository): HttpRoutes[IO] = HttpRoutes.of[IO] {
    case GET -> Root / "genres" :? NameParamMatcher(name) =>
      Ok(genreRepo.search(name))

    case GET -> Root / "genres" / UUIDVar(id) =>
      genreRepo.getById(id).flatMap(optResponse(_))

    case req @ POST -> Root / "genres" =>
      req.as[GenreDto].flatMap(dto => Created(genreRepo.insert(dto.name)))

    case req @ PUT -> Root / "genres" =>
      req.as[Genre]
        .flatMap(dto => genreRepo.update(dto.id, dto.name))
        .flatMap(optResponse(_))

    case DELETE -> Root / "genres" / UUIDVar(id) =>
      genreRepo.delete(id).flatMap(optResponse(_))
  }

  private def httpApp(transactor: Transactor[IO]): Kleisli[IO, Request[IO], Response[IO]] = {
    bookRoutes(new BookRepository(transactor)) <+>
      authorRoutes(new AuthorRepository(transactor)) <+>
      genreRoutes(new GenreRepository(transactor))
  }.orNotFound

  override def run(args: List[String]): IO[ExitCode] = {
    val resources = for {
      xa <- DbTransactor.make[IO]
      httpServer <-
        BlazeServerBuilder[IO](ExecutionContext.global)
          .bindHttp(port = 9001, host = "localhost")
          .withConnectorPoolSize(1000)
          .withHttpApp(httpApp(xa))
          .resource
    } yield (xa, httpServer)

    resources.use {
      case (xa, httpServer) => for {
        _ <- setup().transact(xa)
        _ <- IO.never
      } yield ExitCode.Success
    }
  }
}
