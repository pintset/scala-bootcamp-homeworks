package effects

import cats.effect.Sync

import scala.util.Random

// TODO: Gen trait can be the same

trait GenInt[F[_]] {
  def generateInt: F[Int]
}

// TODO: Random borders
object GenInt {
  def apply[F[_]: Sync](min: Int, max: Int): GenInt[F] = new GenInt[F] {
    def generateInt: F[Int] = Sync[F].delay(Random.between(min, max))
  }
}
