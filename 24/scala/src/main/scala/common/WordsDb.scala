package common

import cats.effect.{Async, Sync}
import cats.effect.syntax.concurrent._
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.syntax.applicative._
import fs2.io.file.{Files, Path}

import scala.util.Random

trait WordsDb[F[_]] {
  def allWords: F[List[String]]
  def getNext: F[String]
  def isValidWord(word: String): F[Boolean]
}

object WordsDb {
  val guessWords: Path = Path("guessWords.txt")
  val validWords: Path = Path("validWords.txt")

  def apply[F[_]: WordsDb]: WordsDb[F] = implicitly

  def make[F[_]: Async: Files](filePath: Path): F[WordsDb[F]] =
    Files[F].readUtf8Lines(filePath).compile.toList.memoize.map { wordsF =>
      new WordsDb[F] {
        def allWords: F[List[String]] = wordsF
        def getNext: F[String] =
          wordsF
            .flatMap { words => Sync[F].delay(Random.between(0, words.length)).map(words) }
            .flatTap { guess => Sync[F].delay(println(s"Guess: $guess")) }

        def isValidWord(word: String): F[Boolean] =
          wordsF.map(_.contains(word))
      }
    }

//  def make[F[_]: Sync: Files](filePath: Path): F[WordsDb[F]] =
//    Files[F].readUtf8Lines(filePath).compile.toList.map { words =>
//      new WordsDb[F] {
//        def allWords: F[List[String]] = words.pure
//
//        def getNext: F[String] =
//          Sync[F].delay(Random.between(0, words.length))
//            .map(words)
//            .flatTap(guess => Sync[F].delay(println(s"Guess: $guess")))
//
//        def isValidWord(word: String): F[Boolean] =
//          words.contains(word).pure
//      }
//    }
}
