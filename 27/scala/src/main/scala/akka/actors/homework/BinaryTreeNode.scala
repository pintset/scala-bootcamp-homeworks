package akka.actors.homework

import akka.actor.{Actor, ActorRef, Props}
import akka.actors.homework.BinaryTreeSet.Operation._
import akka.actors.homework.BinaryTreeSet.OperationReply.{ContainsResult, OperationFinished}

object BinaryTreeNode {
  private sealed trait Position

  private case object Left extends Position
  private case object Right extends Position

  def props(elem: Int, initiallyRemoved: Boolean): Props = Props(new BinaryTreeNode(elem, initiallyRemoved))
}

final class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {
  import BinaryTreeNode._

  private var subtrees = Map[Position, ActorRef]()
  private var removed = initiallyRemoved

  override def receive: Receive = {
    case m @ Insert(_, _, _) => doInsert(m)
    case m @ Contains(_, _, _) => doContains(m)
    case m @ Remove(_, _, _) => doRemove(m)
  }

  private def doInsert(m: Insert): Unit = {
    def insert(pos: Position): Unit =
      subtrees.get(pos)
        .map(node => node ! m)
        .getOrElse {
          subtrees = subtrees + (pos -> context.actorOf(BinaryTreeNode.props(m.elem, initiallyRemoved = false)))
          m.requester ! OperationFinished(m.id)
        }

    if (m.elem == elem) {
      removed = false
      m.requester ! OperationFinished(m.id)
    } else if (m.elem < elem)
      insert(Left)
    else
      insert(Right)
  }

  private def doContains(m: Contains): Unit = {
    val noLeft = !subtrees.contains(Left)
    val noRight = !subtrees.contains(Right)

    if (elem == m.elem)
      m.requester ! ContainsResult(m.id, !removed)
    else if ((noLeft && m.elem < elem) || (noRight && m.elem > elem) || (noLeft && noRight))
      m.requester ! ContainsResult(m.id, false)
    else if (m.elem < elem)
      subtrees(Left) ! m
    else
      subtrees(Right) ! m
  }

  private def doRemove(m: Remove): Unit = {
    if (elem == m.elem) {
      removed = true
      m.requester ! OperationFinished(m.id)
    } else if (m.elem < elem && subtrees.contains(Left))
      subtrees(Left) ! m
    else if (m.elem > elem && subtrees.contains(Right))
      subtrees(Right) ! m
    else
      m.requester ! OperationFinished(m.id)
  }
}
