package server

import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.syntax.flatMap._
import cats.syntax.functor._
import common.domain.{AttemptResult, GameId, GameOver, Greater, Lower, YouWon}
import effects.{Console, GenInt}

trait GameServer[F[_]] {
  def start(min: Int, max: Int, attemptCount: Int): F[GameId]
  def guess(gameId: GameId, number: Int): F[Option[AttemptResult]]
}

// TODO: Тут собственно говоря тоже State. Т.е. что-то что зависит от одного и того же параметра
// В чистых терминах меня должно интересовать только API (старт, гесс) а не что сюда передаётся реф
// реф в данном случае - это деталь реализации.

object GameServer {
  final case class Game(guessedNumber: Int, attemptCount: Int, attemptsLeft: Int) {
    def result(guess: Int): AttemptResult =
      if (guessedNumber == guess) YouWon(attemptCount - attemptsLeft, guessedNumber)
      else if (attemptsLeft == 0) GameOver(guessedNumber)
      else if (guess > guessedNumber) Greater(attemptsLeft)
      else Lower(attemptsLeft)

//    def isOver(guess: Int): Boolean =
//      attemptsLeft == 0 || guessedNumber == guess
  }

  // TODO: Pass function which generates int. The same for gameId
  def of[F[_]: Sync]: F[GameServer[F]] = {
    Ref.of[F, Map[GameId, Game]](Map.empty).map { gameMapRef =>
      new GameServer[F] {
        def start(min: Int, max: Int, attemptCount: Int): F[GameId] =
          for {
            gameId <- GameId.generate
            number <- GenInt(min, max).generateInt // min max (max not included) // MinMax.generateInt
            _ <- Console[F].putStrLn(s"Guessed number: $number")
            _ <- gameMapRef.update(_ + (gameId -> Game(number, attemptCount, attemptCount)))
          } yield gameId

        def guess(gameId: GameId, number: Int): F[Option[AttemptResult]] =
          gameMapRef.modifyMaybe { games =>
            games.get(gameId).map { game =>
              val newGame = game.copy(attemptsLeft = game.attemptsLeft - 1)
              val gameResult = newGame.result(number)

              if (gameResult.gameIsFinished)
                (games - gameId, gameResult)
              else
                (games + (gameId -> newGame), gameResult)
            }
          }
      }
    }
  }
}