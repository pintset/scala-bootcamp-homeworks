//package common
//
//import java.util.UUID
//import cats.effect.Sync
//import cats.syntax.all._
//import common.Protocol._
//import io.circe.generic.auto._
//
//trait GameClient[F[_]] {
//  def start(min: Int, max: Int): F[UUID]
//  def guess(id: UUID, min: Int, max: Int): F[String]
//}
//
//object GameClient {
//  def apply[F[_] : Sync](client: WebClient[F]): GameClient[F] = new GameClient[F] {
//    def start(min: Int, max: Int): F[UUID] =
//      client.expect(NewGame(min, max), Start)
//
//    def guess(id: UUID, min: Int, max: Int): F[String] = {
//      val number = min + (max - min) / 2
//      client.expect[Guess, GameResult](Guess(id, number), Guess)
//        .flatMap {
//          case YouWon => Sync[F].pure(s"You won. Number is: ${number.toString}")
//          case Greater => guess(id, min, number)
//          case Lower => guess(id, number, max)
//          case GameOver => Sync[F].pure("Game over")
//          case GameNotFound => Sync[F].pure("Game not found")
//        }
//    }
//  }
//}