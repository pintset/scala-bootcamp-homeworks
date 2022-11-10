package common

import cats.Applicative
import cats.data.StateT
import common.domain.{Greater, Lower}

object BotStrategy {
  final case class MinMax(min: Int, max: Int)

  def apply[F[_] : Applicative]: GameStrategy[StateT[F, MinMax, *]] = prevAttemptResultOpt => {
    def strategy(v: MinMax): Int = v.min + (v.max - v.min) / 2

    def nextMinMax(prev: MinMax): MinMax =
      prevAttemptResultOpt.map {
        case Greater(_) => MinMax(prev.min, strategy(prev))
        case Lower(_)   => MinMax(strategy(prev), prev.max)
        case _          => prev
      }.getOrElse(prev)

    StateT
      // Эта штука должна быть впереди потому что стратегия считает
      // nextGuess для новой пары, а не для старой
      .modify(nextMinMax)
      .inspect(strategy)
  }
}