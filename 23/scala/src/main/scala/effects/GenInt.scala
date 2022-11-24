package effects

import cats.effect.Sync
import scala.util.Random

trait GenInt[F[_]] {
  def generateInt: F[Int]
}

object GenInt {
  def apply[F[_]: Sync](min: Int, max: Int): GenInt[F] = new GenInt[F] {
    def generateInt: F[Int] = Sync[F].delay(Random.between(min, max + 1))
  }
}
