package common

import cats.Monad
import cats.data.StateT
import common.domain.{AttemptResult, Greater, Lower}

object BotStrategy {
  final case class MinMax(min: Int, max: Int)

  def apply[F[_]: Monad]: GameStrategy[StateT[F, MinMax, *]] = {
    new GameStrategy[StateT[F, MinMax, *]] {
      def getNext(prevAttemptResultOpt: Option[AttemptResult]): StateT[F, MinMax, Int] = {
        def strategy(min: Int, max: Int): Int = min + (max - min) / 2

        def nextPair(prevMin: Int, prevMax: Int, prevAttemptResultOpt: Option[AttemptResult]): (Int, Int) =
          prevAttemptResultOpt.map {
            case Greater(_) => (prevMin, strategy(prevMin, prevMax))
            case Lower(_) => (strategy(prevMin, prevMax), prevMax)
            case _ => (prevMin, prevMax)
          }.getOrElse((prevMin, prevMax))

        StateT { (s: MinMax) =>
          val nextMinMax: (Int, Int) = nextPair(s.min, s.max, prevAttemptResultOpt)
          val nextGuess: Int = strategy(nextMinMax._1, nextMinMax._2)
          Monad[F].pure((MinMax(nextMinMax._1, nextMinMax._2), nextGuess))
        }
      }
    }
  }
}