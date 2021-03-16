package adt

import Types._

object Poker extends App {
  def sortHands(board: Board, hands: List[Hand]): List[Hand] = {
    def evaluateRank(hand: Hand): Int = Combinations.get(board, hand).map(Evaluate.rank).max
    hands.map { h => (h, evaluateRank(h)) }.sortBy(_._2).map(_._1)
//    hands.map { h => (h, evaluateRank(h)) }.sortBy(_._2).map { case (hand, rank) => (rank match {
//      case rank if rank >= 0x800000 => "StraightFlush"
//      case rank if rank >= 0x700000 => "FourOfAKind"
//      case rank if rank >= 0x600000 => "FullHouse"
//      case rank if rank >= 0x500000 => "Flush"
//      case rank if rank >= 0x400000 => "Straight"
//      case rank if rank >= 0x300000 => "ThreeOfAKind"
//      case rank if rank >= 0x200000 => "TwoPairs"
//      case rank if rank >= 0x100000 => "Pair"
//      case _ => "Highcard"
//    }, hand)}
  }
}