package common

import cats.Applicative
import cats.data.StateT
import cats.implicits.toFunctorOps
import common.domain.{AttemptResult, Greater, Lower, Move}

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
  
  def move[F[_] : Applicative](guess: Int => F[AttemptResult]): Move[StateT[F, MoveState, *]] = {
    // F[_] ~> G[_] where G[A] = Move[StateT[F, MoveState, A]]
    val liftedGuess =
      guess.map { fa => StateT { move: MoveState => fa.map { a => (move.copy(attemptResultOpt = Option(a)), a) } } }

    Move(BotStrategy[F], liftedGuess)
  }
}