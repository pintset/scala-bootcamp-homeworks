package common

import cats.Applicative
import cats.data.StateT
import common.domain.{AttemptResult, Greater, Lower}
import http.GuessClient.Move

object BotStrategy {
  final case class MoveState(min: Int, max: Int, attemptResultOpt: Option[AttemptResult])

  def apply[F[_] : Applicative]: GameStrategy[StateT[F, MoveState, *]] = {
    def strategy(min: Int, max: Int): Int = min + (max - min) / 2

    def nextMinMax(prev: MoveState): MoveState =
      prev.attemptResultOpt.map {
        case Greater(_) => prev.copy(max = strategy(prev.min, prev.max))
        case Lower(_)   => prev.copy(min = strategy(prev.min, prev.max))
        case _          => prev
      }.getOrElse(prev)

    StateT
      .modify(nextMinMax)
      .inspect(s => strategy(s.min, s.max))
  }

  // Переименовать в гейм?
  def move[F[_] : Applicative](guess: Int => StateT[F, MoveState, AttemptResult]): Move[StateT[F, MoveState, *]] =
    Move(BotStrategy[F], guess)
}