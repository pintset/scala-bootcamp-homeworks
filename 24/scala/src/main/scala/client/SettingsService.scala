package client

import cats.Applicative
import cats.effect.Sync
import cats.syntax.apply._
import cats.syntax.functor._
import cats.effect.std.Console
import common.domain.NewGame

trait SettingsService[F[_], A] {
  def getSettings: F[NewGame[A]]
}

object SettingsService {
  def apply[F[_]: Applicative, A]: SettingsService[F, A] = new SettingsService[F, A] {
    def getSettings: F[NewGame[A]] = Applicative[F].pure(NewGame(6))
  }

  def console[F[_]: Sync: Console, A]: SettingsService[F, A] =
    new SettingsService[F, A] {
      def getSettings: F[NewGame[A]] = {
        import common.Console
          Console.inputIntWithRetry("Enter number of attempts: ").map(NewGame[A])
      }
    }
}