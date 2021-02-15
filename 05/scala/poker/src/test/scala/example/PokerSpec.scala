package example

import adt.Poker
import adt.Types._
import org.scalatest.Assertion
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PokerSpec extends AnyFlatSpec with Matchers {
  def pokerAssert(board: Either[Error, Board], handsOriginalAndSorted: Either[Error, (List[Hand], List[Hand])]): Assertion =
    assertResult(handsOriginalAndSorted.map(_._2)) {
      for {
        b <- board
        hs <- handsOriginalAndSorted
      } yield Poker.sortHands(b, hs._1)
    }

  "A Poker.sortHands" should "return hands in the order: Ad4s Ac4d 5d6d As9s KhKd" in {
    val board = Board(List(
      Card(Rank.Four, Suit.Clubs),
      Card(Rank.King, Suit.Spades),
      Card(Rank.Four, Suit.Hearts),
      Card(Rank.Eight, Suit.Spades),
      Card(Rank.Seven, Suit.Spades)
    ))

    val handsOriginalAndSorted = for {
      h1 <- Hand(List(Card(Rank.Ace, Suit.Diamonds), Card(Rank.Four, Suit.Spades)))
      h2 <- Hand(List(Card(Rank.Ace, Suit.Clubs), Card(Rank.Four, Suit.Diamonds)))
      h3 <- Hand(List(Card(Rank.Ace, Suit.Spades), Card(Rank.Nine, Suit.Spades)))
      h4 <- Hand(List(Card(Rank.King, Suit.Hearts), Card(Rank.King, Suit.Diamonds)))
      h5 <- Hand(List(Card(Rank.Five, Suit.Diamonds), Card(Rank.Six, Suit.Diamonds)))
    } yield (List(h1, h2, h3, h4, h5), List(h1, h2, h5, h3, h4))

    pokerAssert(board, handsOriginalAndSorted)
  }

  it should "return hands in the order: KdKs 9hJh" in {
    val board = Board(List(
      Card(Rank.Two, Suit.Hearts),
      Card(Rank.Three, Suit.Hearts),
      Card(Rank.Four, Suit.Hearts),
      Card(Rank.Five, Suit.Diamonds),
      Card(Rank.Eight, Suit.Diamonds)
    ))

    val handsOriginalAndSorted = for {
      h1 <- Hand(List(Card(Rank.King, Suit.Diamonds), Card(Rank.King, Suit.Spades)))
      h2 <- Hand(List(Card(Rank.Nine, Suit.Hearts), Card(Rank.Jack, Suit.Hearts)))
    } yield (List(h1, h2), List(h1, h2))

    pokerAssert(board, handsOriginalAndSorted)
  }

  it should "return hands in the order: Qc8dAd6c KsAsTcTs Js2dKd8c 7dQsAc5d Jh2h3c9c" in {
    val board = Board(List(
      Card(Rank.Three, Suit.Diamonds),
      Card(Rank.Three, Suit.Spades),
      Card(Rank.Four, Suit.Diamonds),
      Card(Rank.Six, Suit.Hearts),
      Card(Rank.Jack, Suit.Clubs)
    ))

    val handsOriginalAndSorted = for {
      h1 <- Hand(List(Card(Rank.Jack, Suit.Spades), Card(Rank.Two, Suit.Diamonds), Card(Rank.King, Suit.Diamonds), Card(Rank.Eight, Suit.Clubs)))
      h2 <- Hand(List(Card(Rank.King, Suit.Spades), Card(Rank.Ace, Suit.Spades), Card(Rank.Ten, Suit.Clubs), Card(Rank.Ten, Suit.Spades)))
      h3 <- Hand(List(Card(Rank.Jack, Suit.Hearts), Card(Rank.Two, Suit.Hearts), Card(Rank.Three, Suit.Clubs), Card(Rank.Nine, Suit.Clubs)))
      h4 <- Hand(List(Card(Rank.Queen, Suit.Clubs), Card(Rank.Eight, Suit.Diamonds), Card(Rank.Ace, Suit.Diamonds), Card(Rank.Six, Suit.Clubs)))
      h5 <- Hand(List(Card(Rank.Seven, Suit.Diamonds), Card(Rank.Queen, Suit.Spades), Card(Rank.Ace, Suit.Clubs), Card(Rank.Five, Suit.Diamonds)))
    } yield (List(h1, h2, h3, h4, h5), List(h4, h2, h1, h5, h3))

    pokerAssert(board, handsOriginalAndSorted)
  }

  it should "fail with error InvalidNumberOfCards" in {
    assertResult(Left(Error.InvalidNumberOfCards(6, 5))) {
      Board(List(
        Card(Rank.Three, Suit.Diamonds),
        Card(Rank.Three, Suit.Spades),
        Card(Rank.Four, Suit.Diamonds),
        Card(Rank.Six, Suit.Hearts),
        Card(Rank.Jack, Suit.Clubs),
        Card(Rank.Ace, Suit.Clubs)
      ))
    }
  }

  it should "fail with error DuplicateCardsDetected" in {
    assertResult(Left(Error.DuplicateCardsDetected(List(Card(Rank.Three, Suit.Diamonds))))) {
      Board(List(
        Card(Rank.Three, Suit.Diamonds),
        Card(Rank.Three, Suit.Spades),
        Card(Rank.Four, Suit.Diamonds),
        Card(Rank.Three, Suit.Diamonds),
        Card(Rank.Jack, Suit.Clubs),
      ))
    }
  }
}
