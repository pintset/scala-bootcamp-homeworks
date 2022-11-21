package client

import cats.Applicative
import cats.effect.Sync
import effects.Console
import cats.syntax.apply._
import common.domain.NewGame

trait SettingsService[F[_]] {
  def getSettings: F[NewGame]
}

object SettingsService {
  def apply[F[_]: Applicative]: SettingsService[F] = new SettingsService[F] {
    def getSettings: F[NewGame] = Applicative[F].pure(NewGame(10, 100, 5))
  }

  def console[F[_] : Sync]: SettingsService[F] = new SettingsService[F] {
    def getSettings: F[NewGame] = {
      (Console.inputInt("Enter min number: "),
        Console.inputInt("Enter max number: "),
        Console.inputInt("Enter number of attempts: "))
        .mapN { (min, max, attemptCount) => NewGame(min, max, attemptCount) }
    }
  }
}
