package client

import cats.Applicative
import cats.effect.Sync
import cats.syntax.apply._
import cats.effect.std.Console
import common.domain.NewGame

trait SettingsService[F[_]] {
  def getSettings: F[NewGame]
}

object SettingsService {
  def apply[F[_] : Applicative]: SettingsService[F] = new SettingsService[F] {
    def getSettings: F[NewGame] = Applicative[F].pure(NewGame(0, 100, 5))
  }

  def console[F[_] : Sync: Console]: SettingsService[F] =
    new SettingsService[F] {
      def getSettings: F[NewGame] = {
        import common.Console
        (Console.inputIntWithRetry("Enter min number: "),
          Console.inputIntWithRetry("Enter max number: "),
          Console.inputIntWithRetry("Enter number of attempts: "))
          .mapN { (min, max, attemptCount) => NewGame(min, max, attemptCount) }
      }
    }
}