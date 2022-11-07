package client

import cats.effect.Sync
import common.domain.NewGame
import effects.Console
import cats.syntax.apply._

trait SettingsService[F[_]] {
  def getSettings: F[NewGame]
}

object SettingsService {
  def console[F[_] : Sync]: SettingsService[F] = new SettingsService[F] {
    def getSettings: F[NewGame] = {
      (Console.inputInt("Enter min number: "),
        Console.inputInt("Enter max number: "),
        Console.inputInt("Enter number of attempts: "))
        .mapN { (min, max, attemptCount) => NewGame(min, max, attemptCount) }
    }
  }
}
