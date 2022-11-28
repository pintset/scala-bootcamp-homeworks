package server

import common.domain.AttemptResult

trait GameResult[A] {
  def result(game: Game[A], guess: A): AttemptResult[A]
}

object GameResult {
  def apply[A](implicit gr: GameResult[A]): GameResult[A] = gr

  implicit class GameResultOps[A: GameResult](game: Game[A]) {
    def result(guess: A): AttemptResult[A] = GameResult[A].result(game, guess)
  }
}