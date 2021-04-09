package example

import cats.effect.{Blocker, ContextShift, ExitCode, IO, IOApp, Resource, Sync}

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import scala.io.{BufferedSource, Source}
import cats.syntax.parallel._

object MinTextHash extends IOApp {
  def javaHash(word: String, seed: Int = 0): Int = {
    var hash = 0

    for (ch <- word.toCharArray)
      hash = 31 * hash + ch.toInt

    hash = hash ^ (hash >> 20) ^ (hash >> 12)
    hash ^ (hash >> 7) ^ (hash >> 4)
  }

  def knuthHash(word: String, constant: Int): Int = {
    var hash = 0
    for (ch <- word.toCharArray)
      hash = ((hash << 5) ^ (hash >> 27)) ^ ch.toInt
    hash % constant
  }

  def getSource(blocker: Blocker)(implicit contextShift: ContextShift[IO], sync: Sync[IO]): IO[BufferedSource] =
    blocker.delay {
      // println(s"operation - ${ Thread.currentThread().getName }")
      print("Please enter file path: ")
      Source.fromFile(scala.io.StdIn.readLine())
    }.handleErrorWith(_ => IO(println("Unable to open file")) *> getSource(blocker))

  def getSeed(blocker: Blocker)(implicit contextShift: ContextShift[IO], sync: Sync[IO]): IO[Int] =
    blocker.delay {
      // println(s"operation - ${ Thread.currentThread().getName }")
      print("Please enter seed: ")
      scala.io.StdIn.readLine().toInt
    }.handleErrorWith(_ => IO(println("Unable to parse seed")) *> getSeed(blocker))

  def getHashes(word: String, seed: Int, hashFuncs: List[(String, Int) => Int])(implicit contextShift: ContextShift[IO]): IO[List[Int]] =
  // hashFuncs.parTraverse(hashFunc => IO(println(Thread.currentThread().getName)) *> IO(hashFunc(word, seed)))
    hashFuncs.parTraverse(hashFunc => IO(hashFunc(word, seed)))

  def getMinHashes(words: List[String], seed: Int, hashFuncs: List[(String, Int) => Int])(implicit contextShift: ContextShift[IO]): IO[List[Int]] =
    words.parTraverse(getHashes(_, seed, hashFuncs)).map { ll =>
      ll.reduceLeft[List[Int]] { case (minHashes, hashes) =>
        minHashes.zip(hashes).map { case (e1, e2) => Math.min(e1, e2) }
      }
    }

  def mainIO(source: BufferedSource, seed: Int)(implicit contextShift: ContextShift[IO]): IO[ExitCode] = {
    val words = """([A-Za-z])+""".r
    val hashFuncs: List[(String, Int) => Int] = List(javaHash, knuthHash)

    for {
      _ <- IO.shift
      minHashes <- getMinHashes(source.getLines().flatMap(words.findAllIn).toList, seed, hashFuncs)
      _ <- IO(println("Hashes:"))
      _ <- IO(minHashes.foreach(println))
    } yield ExitCode.Success
  }

  override def run(args: List[String]): IO[ExitCode] = {
    val executor = Executors.newFixedThreadPool(10, (r: Runnable) => {
      val t = Executors.defaultThreadFactory().newThread(r)
      t.setDaemon(true)
      t
    })
    val cs = IO.contextShift(ExecutionContext.fromExecutor(executor))

    (for {
      source <- Resource.fromAutoCloseable(Blocker[IO].use(getSource))
      seed <- Resource.make(Blocker[IO].use(getSeed))(_ => IO.unit)
    } yield (source, seed)).use { case (source, seed) => mainIO(source, seed)(cs) }
  }
}
