package server

import cats.Monad
import cats.effect.Sync
import cats.effect.Ref
import cats.syntax.flatMap._
import cats.syntax.functor._
import common.domain.{AttemptResult, GameId}
import effects.GenUUID
import GameResult.GameResultOps

trait GameServer[F[_], A] {
  def start(attemptCount: Int): F[GameId]
  def guess(gameId: GameId, guess: A): F[Option[AttemptResult[A]]]
}

object GameServer {
  def of[F[_]: Sync: GenUUID, A: GameResult](getNextGuess: F[A]): F[GameServer[F, A]] = {
    def modifyMaybe[C, D](ref: Ref[F, C])(f: C => Option[(C, D)])(implicit F: Monad[F]): F[Option[D]] =
      ref.access.flatMap {
        case (a, set) =>
          f(a) match {
            case Some((a, b)) => set(a).ifM(ifTrue = F.pure(Some(b)), ifFalse = modifyMaybe(ref)(f))
            case None => F.pure(Option.empty[D])
          }
      }

    Ref.of[F, Map[GameId, Game[A]]](Map.empty).map { gameMapRef =>
      new GameServer[F, A] {
        def start(attemptCount: Int): F[GameId] = {
          for {
            gameId <- GameId.generate
            guess <- getNextGuess

            _ <- gameMapRef.update(_ + (gameId -> Game(guess, attemptCount, attemptCount)))
          } yield gameId
        }

        def guess(gameId: GameId, guess: A): F[Option[AttemptResult[A]]] =
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