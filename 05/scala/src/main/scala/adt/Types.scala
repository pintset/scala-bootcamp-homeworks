package adt

object Types {
  sealed trait Suit
  object Suit {
    final case object Clubs extends Suit
    final case object Diamonds extends Suit
    final case object Hearts extends  Suit
    final case object Spades extends Suit
  }

  sealed trait Rank
  object Rank {
    final case object Two extends Rank
    final case object Three extends Rank
    final case object Four extends Rank
    final case object Five extends Rank
    final case object Six extends Rank
    final case object Seven extends Rank
    final case object Eight extends Rank
    final case object Nine extends Rank
    final case object Ten extends Rank
    final case object Jack extends Rank
    final case object Queen extends Rank
    final case object King extends Rank
    final case object Ace extends Rank
  }

  final case class Card(rank: Rank, suit: Suit)

  sealed trait Error
  object Error {
    final case class InvalidNumberOfCards(actual: Int, expected: Int) extends Error
    final case class DuplicateCardsDetected(cards: List[Card]) extends Error
    final case class UnsupportedHandSize(size: Int) extends Error
  }

  final case class Card2 private (cards: List[Card]) extends AnyVal
  final case class Card4 private (cards: List[Card]) extends AnyVal
  final case class Card5 private (cards: List[Card]) extends AnyVal

  object CardN {
    def apply[A](f: List[Card] => A, n: Int, cards: List[Card]): Either[Error, A] = cards.length match {
      case len if len != n => Left(Error.InvalidNumberOfCards(len, n))
      case _ if cards.distinct.length != n => Left(Error.DuplicateCardsDetected(
        Evaluate.countBy(cards)(identity).filter { case (_, count) => count > 1 }.keys.toList
      ))
      case _ => Right(f(cards))
    }
  }

  object Card2 {
    def apply(cards: List[Card]): Either[Error, Card2] = CardN(new Card2(_), 2, cards)
  }

  object Card4 {
    def apply(cards: List[Card]): Either[Error, Card4] = CardN(new Card4(_), 4, cards)
  }

  object Card5 {
    def apply(cards: List[Card]): Either[Error, Card5] = CardN(new Card5(_), 5, cards)
  }

  sealed trait Hand
  object Hand {
    final case class Texas(cards: Card2) extends Hand
    final case class Omaha(cards: Card4) extends Hand

    def apply(cards: List[Card]): Either[Error, Hand] = cards.length match {
      case 2 => Card2(cards).map(Texas)
      case 4 => Card4(cards).map(Omaha)
      case x => Left(Error.UnsupportedHandSize(x))
    }
  }

  final case class Board(cards: Card5)
  object Board {
    def apply(cards: List[Card]): Either[Error, Board] = Card5(cards).map(new Board(_))
  }
}
