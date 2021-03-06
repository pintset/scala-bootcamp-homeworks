package crud

import java.util.UUID

object DbCommon {

  val authorOdersky: UUID = UUID.randomUUID()
  val authorRowling: UUID = UUID.randomUUID()
  val genreFantasy: UUID = UUID.randomUUID()
  val genreTechnology: UUID = UUID.randomUUID()
  val bookScala: UUID = UUID.randomUUID()
  val bookHPStone: UUID = UUID.randomUUID()
  val bookHPSecrets: UUID = UUID.randomUUID()

  val createTableAuthorsSql: String =
    """CREATE TABLE authors (
      |  id UUID PRIMARY KEY,
      |  name VARCHAR(100) NOT NULL,
      |  birthday DATE);""".stripMargin

  val createTableGenresSql: String =
    """CREATE TABLE genres (
      |  id UUID PRIMARY KEY,
      |  name VARCHAR(100) NOT NULL);""".stripMargin

  val createTableBooksSql: String =
    """CREATE TABLE books (
      |  id UUID PRIMARY KEY,
      |  author UUID NOT NULL,
      |  genre UUID NOT NULL,
      |  title VARCHAR(100) NOT NULL,
      |  year INT,
      |  FOREIGN KEY (author) REFERENCES authors(id));""".stripMargin

  val populateDataSql: String =
    s"""
       |INSERT INTO authors (id, name, birthday) VALUES
       |  ('$authorOdersky', 'Martin Odersky', '1958-09-05'),
       |  ('$authorRowling', 'J.K. Rowling', '1965-07-31');
       |
       |INSERT INTO genres (id, name) VALUES
       |  ('$genreFantasy', 'Fantasy'),
       |  ('$genreTechnology', 'Technology');
       |
       |INSERT INTO books (id, author, title, genre, year) VALUES
       |  ('$bookScala', '$authorOdersky', 'Programming in Scala', '$genreTechnology', 2016),
       |  ('$bookHPStone', '$authorRowling', 'Harry Potter and Philosopher''s Stone', '$genreFantasy', 1997),
       |  ('$bookHPSecrets', '$authorRowling', 'Harry Potter and the Chamber of Secrets', '$genreFantasy', 1998);
       |""".stripMargin
}
