package crud

import java.time.{LocalDate, Year}
import java.util.UUID
import cats.effect._
import cats.implicits._
import doobie._
import doobie.implicits._
import doobie.implicits.javatime._
import DbCommon._

object Data {
  def setup(): ConnectionIO[Unit] =
    for {
      _ <- Fragment.const(createTableAuthorsSql).update.run
      _ <- Fragment.const(createTableGenresSql).update.run
      _ <- Fragment.const(createTableBooksSql).update.run
      _ <- Fragment.const(populateDataSql).update.run
    } yield ()

  object Common {
    implicit val uuidMeta: Meta[UUID] = Meta[String].timap(UUID.fromString)(_.toString)
    implicit val yearMeta: Meta[Year] = Meta[Int].timap(Year.of)(_.getValue)

    def byId(id: UUID): Fragment = fr"WHERE id = $id"

    def byName(name: Option[String]): Option[Fragment] =
      name.map(n => s"%$n%").map(n => fr"name LIKE $n")
  }
}

class AuthorRepository(transactor: Transactor[IO]) {
  import Data.Common._

  private val authors: Fragment =
    fr"SELECT id, name, birthday FROM authors"

  private def fetchById(id: UUID): ConnectionIO[Option[Author]] =
    (authors ++ byId(id)).query[Author].option

  def search(name: Option[String], birthday: Option[LocalDate]): IO[List[Author]] = {
    val byBirthday = birthday.map(x => fr"birthday = $x")

    (authors ++ Fragments.whereAndOpt(byName(name), byBirthday))
      .query[Author].to[List]
      .transact(transactor)
  }

  def getById(id: UUID): IO[Option[Author]] =
    fetchById(id).transact(transactor)

  def insert(name: String, birthday: LocalDate): IO[Option[Author]] = {
    val id = UUID.randomUUID()
    fr"INSERT INTO authors (id, name, birthday) VALUES ($id, $name, $birthday)".update.run.as(id).flatMap(fetchById)
      .transact(transactor)
  }

  def update(id: UUID, name: String, birthday: LocalDate): IO[Option[Author]] =
    (fr"UPDATE authors SET name = $name, birthday = $birthday" ++ byId(id)).update.run.flatMap { rowsAffected =>
      val none: Option[Author] = None
      if (rowsAffected == 1) fetchById(id) else none.pure[ConnectionIO]
    }.transact(transactor)

  def delete(id: UUID): IO[Option[Unit]] =
    (fr"DELETE FROM authors" ++ byId(id)).update.run.map(rowsAffected => if (rowsAffected == 1) Some() else None)
      .transact(transactor)
}

class GenreRepository(transactor: Transactor[IO]) {
  import Data.Common._

  val genres: Fragment =
    fr"SELECT id, name FROM genres"

  private def fetchById(id: UUID): ConnectionIO[Option[Genre]] =
    (genres ++ byId(id)).query[Genre].option

  def search(name: Option[String]): IO[List[Genre]] =
    (genres ++ Fragments.whereAndOpt(byName(name))).query[Genre].to[List]
      .transact(transactor)

  def getById(id: UUID): IO[Option[Genre]] =
    fetchById(id).transact(transactor)

  def insert(name: String): IO[Option[Genre]] = {
    val id = UUID.randomUUID()
    fr"INSERT INTO genres (id, name) VALUES ($id, $name)".update.run.as(id).flatMap(fetchById)
      .transact(transactor)
  }

  def update(id: UUID, name: String): IO[Option[Genre]] =
    (fr"UPDATE genres SET name = $name" ++ byId(id)).update.run.flatMap { rowsAffected =>
      val none: Option[Genre] = None
      if (rowsAffected == 1) fetchById(id) else none.pure[ConnectionIO]
    }.transact(transactor)

  def delete(id: UUID): IO[Option[Unit]] =
    (fr"DELETE FROM genres" ++ byId(id)).update.run.map(rowsAffected => if (rowsAffected == 1) Some() else None)
      .transact(transactor)
}

class BookRepository(transactor: Transactor[IO]) {
  import Data.Common._

  private val books: Fragment =
    fr"SELECT id, author, title, genre, year FROM books"

  private val booksFull: Fragment =
    fr"""SELECT b.id, a.id, a.name, a.birthday, b.title, g.id, g.name, b.year FROM books b
          INNER JOIN authors a ON b.author = a.id
          INNER JOIN genres g ON b.genre = g.id"""

  private def fetchById(id: UUID): ConnectionIO[Option[Book]] =
    (books ++ byId(id)).query[Book].option

  def search(title: Option[String], year: Option[Year], author: Option[String], genre: Option[String]): IO[List[BookFull]] = {
    val byTitle = title.map(x => s"%$x%").map(x => fr"b.title LIKE $x")
    val byYear = year.map(x => fr"b.Year = $x")
    val byAuthor = author.map(x => s"%$x%").map(x => fr"a.name LIKE $x")
    val byGenre = genre.map(x => s"%$x%").map(x => fr"g.name LIKE $x")

    (booksFull ++ Fragments.whereAndOpt(byTitle, byYear, byAuthor, byGenre)).query[BookFull].to[List]
      .transact(transactor)
  }

  def getById(id: UUID): IO[Option[Book]] =
    fetchById(id).transact(transactor)

  def insert(title: String, authorId: UUID, genreId: UUID, year: Year): IO[Option[Book]] = {
    val id = UUID.randomUUID()
    fr"INSERT INTO books (id, title, author, genre, year) VALUES ($id, $title, $authorId, $genreId, $year)"
      .update.run.as(id).flatMap(fetchById)
      .transact(transactor)
  }

  def update(id: UUID, title: String, authorId: UUID, genreId: UUID, year: Year): IO[Option[Book]] =
    fr"UPDATE books SET title = $title, author = $authorId, genre = $genreId, year = $year WHERE id = $id"
      .update.run.flatMap { rowsAffected =>
      val none: Option[Book] = None
      if (rowsAffected == 1) fetchById(id) else none.pure[ConnectionIO]
    }.transact(transactor)

  def delete(id: UUID): IO[Option[Unit]] =
    (fr"DELETE FROM books" ++ byId(id)).update.run.map(rowsAffected => if (rowsAffected == 1) Some() else None)
      .transact(transactor)
}