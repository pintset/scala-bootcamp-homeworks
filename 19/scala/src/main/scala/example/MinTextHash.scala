package example

import concurrent.ExecutionContext
import java.util.concurrent._

import cats.effect._

import scala.io.{StdIn, Source, BufferedSource}
import scala.util.Try

import cats.data.{EitherT, NonEmptySeq}
import java.nio.file.{Path, Paths}

import cats.syntax.all._
import cats.effect.syntax.all._

object MinTextHash extends IOApp {
  type HashFunc = (String, Int) => Int
  object Hash {
    def java(word: String, seed: Int = 0): Int = {
      var hash = 0

      for (ch <- word.toCharArray)
        hash = 31 * hash + ch.toInt

      hash = hash ^ (hash >> 20) ^ (hash >> 12)
      hash ^ (hash >> 7) ^ (hash >> 4)
    }

    def knuth(word: String, constant: Int): Int = {
      var hash = 0
      for (ch <- word.toCharArray)
        hash = ((hash << 5) ^ (hash >> 27)) ^ ch.toInt
      hash % constant
    }
  }

  def putStr[F[_]: Sync: ContextShift](blocker: Blocker)(text: String): F[Unit] =
    blocker.delay(println(s"${Thread.currentThread.getName}: $text"))

  def threadName[F[_]: Sync]: F[Unit] = {
    Sync[F].delay(println(s"${Thread.currentThread.getName} - ${Thread.currentThread.getId}"))
  }

  def prompt[F[_]: Sync: ContextShift](blocker: Blocker)(text: String): F[String] =
    blocker.delay(StdIn.readLine(s"${Thread.currentThread.getName}: $text"))

  sealed trait FilePathValidationError {
    override def toString: String = this match {
      case InvalidPath(path) => s"Provided path is invalid: $path"
      case FileDoesNotExist(filePath) => s"Specified file does not exist: $filePath"
    }
  }
  final case class InvalidPath(path: String) extends FilePathValidationError
  final case class FileDoesNotExist(filePath: String) extends FilePathValidationError

  final case class FilePath private (filePath: Path) extends AnyVal
  object FilePath {
    def apply(filePath: String): Either[FilePathValidationError, FilePath] = {
      def pathIsValid(filePath: String) =
        Try(Paths.get(filePath)).fold(_ => InvalidPath(filePath).asLeft, _.asRight)

      def fileExists(filePath: Path) = {
        lazy val doesNotExist = FileDoesNotExist(filePath.toString).asLeft
        Try(filePath.toFile.exists).fold(_ => doesNotExist, if (_) filePath.asRight else doesNotExist)
      }

      pathIsValid(filePath).flatMap(fileExists).map(FilePath(_))
    }
  }

  final case class SeedIsNotAnInteger(seed: String) extends AnyVal {
    override def toString: String = s"Specified seed is not an integer: $seed"
  }

  final case class Seed private (value: Int) extends AnyVal
  object Seed {
    def apply(seed: String): Either[SeedIsNotAnInteger, Seed] =
      seed.toIntOption.fold(SeedIsNotAnInteger(seed).asLeft[Seed])(Seed(_).asRight)
  }

  sealed trait AppError {
    override def toString: String = this match {
      case PathError(pathError) => pathError.toString
      case SeedError(seedError) => seedError.toString
      case NoWords => "The provided file does not contain any words"
    }
  }
  final case class PathError(pathError: FilePathValidationError) extends AppError
  final case class SeedError(seedError: SeedIsNotAnInteger) extends AppError
  case object NoWords extends AppError

  def getFilePathAndSeed[F[_]: Sync: ContextShift](blocker: Blocker): EitherT[F, AppError, (FilePath, Seed)] =
    for {
      filePath <- EitherT[F, AppError, FilePath](prompt(blocker)("Enter a file name: ").map(FilePath(_).leftMap(PathError)))
      seed <- EitherT[F, AppError, Seed](prompt(blocker)("Enter seed: ").map(Seed(_).leftMap(SeedError)))
    } yield (filePath, seed)

  def fileResource[F[_]: Sync](file: FilePath): Resource[F, BufferedSource] =
    Resource.fromAutoCloseable(Sync[F].delay(Source.fromFile(file.filePath.toFile)))

  def split(s: String): Seq[String] = {
    val wordsRegex = """([A-Za-z])+""".r
    wordsRegex.findAllIn(s).toSeq
  }

  def readData[F[_]: Sync](resource: Resource[F, BufferedSource]): F[Either[AppError, NonEmptySeq[String]]] =
    resource.use { source => Sync[F].delay(
      NonEmptySeq.fromSeq(
        source.getLines().flatMap(split).toSeq)
        .fold(NoWords.asLeft[NonEmptySeq[String]])(_.asRight)) }

  def minHash(hashFunc: HashFunc, seed: Seed): NonEmptySeq[String] => Int = words => words.map(hashFunc(_, seed.value)).minimum

  def reducePar[F[_]: Sync: Concurrent](data: NonEmptySeq[String], reducer: NonEmptySeq[String] => Int): F[Int] = {
    for {
      coreCount <- Runtime.getRuntime.availableProcessors().toDouble.pure[F]
      fibers <- data.grouped(math.ceil(data.length / coreCount).toInt).toList.traverse(words => (threadName *> Sync[F].delay(reducer(words))).start)

      hashes <- fibers.traverse(f => f.join)
    } yield hashes.min
  }

  def calculateMinHash[F[_]: Sync: Concurrent](hashFunc: HashFunc): (FilePath, Seed) => EitherT[F, AppError, Int] = { (filePath, seed) =>
    for {
      data <- EitherT(readData[F](fileResource[F](filePath)))
      hash <- EitherT.liftF(reducePar(data, minHash(hashFunc, seed)))
    } yield hash
  }

  def daemonThreadFactory(name: String): ThreadFactory = r => {
    val t = new Thread(r, name)
    t.setDaemon(true)
    t
  }

  def program[F[_]: ContextShift: Concurrent](hashFuncs: Seq[HashFunc]): F[ExitCode] = {
    val executorService = Executors.newSingleThreadExecutor(daemonThreadFactory("blocker"))
    val blocker = Blocker.liftExecutorService(executorService)

    (for {
      results <-
        (for {
          pathAndSeed <- getFilePathAndSeed[F](blocker)
          hashes <- hashFuncs.traverse(calculateMinHash[F](_).tupled(pathAndSeed))
        } yield hashes).value.onError(e => putStr[F](blocker)(s"Abnormal program termination: $e"))

      _ <- putStr[F](blocker)(results.fold(_.toString, _.mkString(", ")))
    } yield ExitCode.Success).handleErrorWith(_ => Sync[F].pure(ExitCode.Error))
  }

  def run(args: List[String]): IO[ExitCode] = {
    implicit val executionContext = ExecutionContext.fromExecutorService(
      Executors.newFixedThreadPool(6, daemonThreadFactory("compute"))
    )
    implicit lazy val contextShift: ContextShift[IO] = IO.contextShift(executionContext)
    implicit lazy val timer: Timer[IO] = IO.timer(executionContext)

    program[IO](Seq(Hash.java, Hash.knuth))
  }
}