package adt

import Types.{Board, Hand, Card}

object Combinations {
  private def choose[A](n: List[A], k: Int): List[List[A]] = n.toSet.subsets(k).map(_.toList).toList

  private def texasHoldem(board: List[Card], hand: List[Card]): List[List[Card]] = {
    val singles = for {
      card <- hand
      comb <- choose(board, 4)
    } yield card :: comb

    val doubles = for {
      comb <- choose(board, 3)
    } yield hand.appendedAll(comb)

    (board :: singles).appendedAll(doubles)
  }

  private def omahaHoldem(board: List[Card], hand: List[Card]): List[List[Card]] =
    for {
      hand <- choose(hand, 2)
      comb <- choose(board, 3)
    } yield hand.appendedAll(comb)

  def get(board: Board, hand: Hand): List[List[Card]] = hand match {
    case Hand.Texas(texas) => texasHoldem(board.cards.cards, texas.cards)
    case Hand.Omaha(omaha) => omahaHoldem(board.cards.cards, omaha.cards)
  }
}
