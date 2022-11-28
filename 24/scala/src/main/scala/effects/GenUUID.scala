package effects

import cats.effect.Sync
import java.util.UUID

trait GenUUID[F[_]] {
  def createUUID: F[UUID]
}

object GenUUID {
  def make[F[_]: Sync]: GenUUID[F] = new GenUUID[F] {
    def createUUID: F[UUID] =  Sync[F].delay(UUID.randomUUID)
  }
}
