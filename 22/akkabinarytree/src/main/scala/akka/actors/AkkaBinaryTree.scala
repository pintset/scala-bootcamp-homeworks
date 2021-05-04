package akka.actors

object AkkaBinaryTree extends App {
  import akka.actor.{Actor, ActorRef, ActorSystem, Props}
  import NodeActor._

  final class NodeActor extends Actor {
    private var leaf: Int = 0
    private var removed: Boolean = false

    private def leftNode: Option[ActorRef] =
      context.child("left")

    private def rightNode: Option[ActorRef] =
      context.child("right")

    override def receive: Receive = {
      case Create(requester, id, element) =>
        leaf = element
        requester ! OperationFinished(id, element)

      case Insert(requester, id, element) =>
        def insert(name: String, nodeOpt: Option[ActorRef]): Unit =
          nodeOpt
            .map(node => node ! Insert(requester, id, element))
            .getOrElse {
              val node = context.actorOf(Props[NodeActor], name)
              node ! Create(requester, id, element)
            }

        if (element == leaf) {
          removed = false
          requester ! OperationFinished(id, element)
        } else if (element < leaf)
          insert("left", leftNode)
        else
          insert("right", rightNode)

      case Contains(requester, id, element) =>
        if (leaf == element)
          requester ! ContainsResult(id, !removed, element)
        else if ((leftNode.isEmpty && element < leaf) || (rightNode.isEmpty && element > leaf) || (leftNode.isEmpty && rightNode.isEmpty))
          requester ! ContainsResult(id, false, element)
        else if (element < leaf)
          leftNode.get ! Contains(requester, id, element)
        else
          rightNode.get ! Contains(requester, id, element)

      case Remove(requester, id, element) =>
        if (leaf == element) {
          removed = true
          requester ! OperationFinished(id, element)
        } else if (element < leaf && leftNode.isDefined)
          leftNode.get ! Remove(requester, id, element)
        else if (element > leaf && rightNode.isDefined)
          rightNode.get ! Remove(requester, id, element)
        else
          requester ! OperationFinished(id, element)
    }
  }

  object NodeActor {
    sealed trait In
    final case class Create  (requester: ActorRef, id: Int, element: Int) extends In
    final case class Insert  (requester: ActorRef, id: Int, element: Int) extends In
    final case class Contains(requester: ActorRef, id: Int, element: Int) extends In
    final case class Remove  (requester: ActorRef, id: Int, element: Int) extends In

    sealed trait Out
    final case class OperationFinished(id: Int, element: Int)
    final case class ContainsResult(id: Int, contains: Boolean, element: Int)
  }

  final class Main extends Actor {
    private val tree: ActorRef = context.actorOf(Props[NodeActor], "tree")

    private var ops = List(
      Create(self, 1, 7),
      Insert(self, 1, 3),
      Insert(self, 1, 2),
      Insert(self, 1, 5),
      Insert(self, 1, 9),
      Insert(self, 1, 8),
      Insert(self, 1, 10),
      Insert(self, 1, 6),
      Insert(self, 1, 1),

      Contains(self, 1, 7),
      Contains(self, 1, 3),
      Contains(self, 1, 2),
      Contains(self, 1, 5),
      Contains(self, 1, 9),
      Contains(self, 1, 8),
      Contains(self, 1, 10),
      Contains(self, 1, 6),
      Contains(self, 1, 1),

      Contains(self, 2, 4),
      Contains(self, 2, 0),
      Contains(self, 2, 11),

      Remove(self, 3, 2),
      Remove(self, 3, 8),

      Contains(self, 3, 2),
      Contains(self, 3, 8),

      Insert(self, 4, 2),
      Insert(self, 4, 8),

      Contains(self, 4, 2),
      Contains(self, 4, 8)
    )

    def doOp(): Unit = ops match {
      case Nil =>
      case op :: tail =>
        ops = tail
        tree ! op
    }

    doOp()

    override def receive: Receive = {
      case OperationFinished(1, element) =>
        println(s"Insert finished: $element")
        doOp()

      case OperationFinished(4, element) =>
        println(s"Insert finished: $element")
        doOp()

      case OperationFinished(3, element) =>
        println(s"Remove finished: $element")
        doOp()

      case ContainsResult(id, contains, element) =>
        println(s"Contains: $contains; id: $id; element: $element")
        doOp()
    }
  }

  val treeActorSystem: ActorSystem = ActorSystem("tree-actor-system")
  treeActorSystem.actorOf(Props[Main]())
}
