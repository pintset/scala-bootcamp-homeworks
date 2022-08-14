package crud

import java.time.{LocalDate, Year}
import java.util.UUID

final case class Author(id: UUID, name: String, birthday: LocalDate)

final case class Genre(id: UUID, name: String)

final case class Book(id: UUID, authorId: UUID, title: String, genreId: UUID, year: Year)

final case class BookFull(id: UUID, author: Author, title: String, genre: Genre, year: Year)