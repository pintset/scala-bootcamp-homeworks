package server

import cats.Monad
import cats.effect.Sync
import cats.effect.Ref
import cats.syntax.flatMap._
import cats.syntax.functor._
import common.domain.{AttemptResult, GameId, GameOver, Greater, Lower, YouWon}
import effects.GenUUID

trait GameServer[F[_]] {
  def start(min: Int, max: Int, attemptCount: Int): F[GameId]
  def guess(gameId: GameId, guess: Int): F[Option[AttemptResult]]
}

object GameServer {
  final case class Game(answer: Int, attemptCount: Int, attemptsLeft: Int) {
    def result(guess: Int): AttemptResult =
      if (answer == guess) YouWon(attemptCount - attemptsLeft, answer)
      else if (attemptsLeft == 0) GameOver(answer)
      else if (guess > answer) Greater(attemptsLeft)
      else Lower(attemptsLeft)
  }

  def of[F[_]: Sync: GenUUID](getNextGuess: (Int, Int) => F[Int]): F[GameServer[F]] = {
    def modifyMaybe[A, B](ref: Ref[F, A])(f: A => Option[(A, B)])(implicit F: Monad[F]): F[Option[B]] =
      ref.access.flatMap {
        case (a, set) =>
          f(a) match {
            case Some((a, b)) => set(a).ifM(ifTrue = F.pure(Some(b)), ifFalse = modifyMaybe(ref)(f))
            case None => F.pure(Option.empty[B])
          }
      }

    Ref.of[F, Map[GameId, Game]](Map.empty).map { gameMapRef =>
      new GameServer[F] {
        def start(min: Int, max: Int, attemptCount: Int): F[GameId] = {
          for {
            gameId <- GameId.generate
            guess <- getNextGuess(min, max)

            _ <- gameMapRef.update(_ + (gameId -> Game(guess, attemptCount, attemptCount)))
          } yield gameId
        }

        def guess(gameId: GameId, guess: Int): F[Option[AttemptResult]] =
          modifyMaybe(gameMapRef) { games =>
            games.get(gameId).map { game =>
              val newGame = game.copy(attemptsLeft = game.attemptsLeft - 1)
              val attemptResult = newGame.result(guess)

              if (attemptResult.gameIsFinished)
                (games - gameId, attemptResult)
              else
                (games + (gameId -> newGame), attemptResult)
            }
          }
      }
    }
  }
}