package adt

import adt.Types.{Card, Rank}

object Evaluate {
  private val ranks : Map[Rank, Int] = Seq(Rank.Two, Rank.Three, Rank.Four, Rank.Five, Rank.Six, Rank.Seven, Rank.Eight,
    Rank.Nine, Rank.Ten, Rank.Jack, Rank.Queen, Rank.King, Rank.Ace).zipWithIndex.toMap

  def countBy[T, S](list:Seq[T])(f: T => S): Map[S, Int] =
    list.groupBy(f).map { case (t, l) => (t, l.size) }

  private def highCard(hand: List[Card]): List[Rank] =
    hand.map(_.rank).sortWith { ranks(_) > ranks(_) }

  object Group {
    def unapply(hand: List[Card]): Option[(List[Int], List[Rank])] =
      Some(countBy(hand)(_.rank).toList.map(_.swap).sortBy { case (count, rank) => (-count, -ranks(rank)) }.unzip)
  }

  object Straight {
    val straights: List[List[Rank]] = (Rank.Ace :: ranks.toList.sortBy(_._2).map(_._1)).sliding(5).map(_.reverse).toList
    def unapply(hand: List[Card]): Option[List[Rank]] = {
      def equalTo[A](l1: List[A], l2: List[A]): Boolean = l2.forall(l1.toSet)
      val handRanks = hand.map(_.rank)
      straights.find { equalTo(handRanks, _) }
    }
  }

  object Flush {
    def unapply(hand: List[Card]): Option[List[Rank]] = {
      countBy(hand)(_.suit).toList match {
        case List(_) => Some(hand.map(_.rank).sortWith { ranks(_) > ranks(_) })
        case _ => None
      }
    }
  }

  object StraightFlush {
    def unapply(hand: List[Card]): Option[List[Rank]] =
      Flush.unapply(hand).flatMap { _ => Straight.unapply(hand) }
  }

  private val hexMap: Map[Rank, Char] = ranks.map { case (r, i) => (r, i.toHexString(0)) }

  private def calc(prefix: Char, ranks: List[Rank]): Int =
    Integer.parseInt((prefix :: ranks.map(hexMap)).mkString, 16)

  def rank(cards: List[Card]): Int = cards match {
    case StraightFlush(ranks) => calc('8', ranks)
    case Group(List(4, 1), List(r, k)) => calc('7', List(r, r, r, r, k))
    case Group(List(3, 2), List(r1, r2)) => calc('6', List(r1, r1, r1, r2, r2))
    case Flush(ranks) => calc('5', ranks)
    case Straight(ranks) => calc('4', ranks)
    case Group(List(3, 1, 1), List(r, k1, k2)) => calc('3', List(r, r, r, k1, k2))
    case Group(List(2, 2, 1), List(r1, r2, k)) => calc('2', List(r1, r1, r2, r2, k))
    case Group(List(2, 1, 1, 1), List(r, k1, k2, k3)) => calc('1', List(r, r, k1, k2, k3))
    case cards => calc('0', highCard(cards))
  }
}
