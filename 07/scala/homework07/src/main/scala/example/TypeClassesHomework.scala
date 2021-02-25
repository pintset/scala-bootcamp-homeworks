package example

object Task1 extends App {
  final case class Money(amount: BigDecimal)

  // Exercise: create Ordering instance for Money
  implicit val moneyOrdering: Ordering[Money] = Ordering.by(_.amount)

  println(List(Money(7), Money(5), Money(10)).sorted)
}

object Task2 extends App {
  trait Show[T] { // fancy toString
    def show(entity: T): String
  }

  // Exercise: create syntax for Show so i can do User("1", "Oleg").show
  object ShowSyntax {
    // Implicit conversion
    implicit class ShowOps[T](x: T) {
      def show(implicit showable: Show[T]): String = showable.show(x)
    }
  }

  final case class User(id: String, name: String)

  // Exercise: create Show instance for User (interface implementation)
  implicit val showUser: Show[User] = user => s"id: ${user.id}; name: ${user.name}"

  import ShowSyntax._
  println(User("1", "Oleg").show)
}

object Task3 extends App {
  type Error = String
  trait Parse[T] { // invent any format you want or it can be csv string
    def parse(entity: String): Either[Error, T]
  }

  // Exercise: create syntax for Parse so i can do "lalala".parse[User] (and get an error because it is obviously not a User)
  object ParseSyntax {
    implicit class ParseOps[T](x: String) {
      def parse[T](implicit parseable: Parse[T]): Either[Error, T] = parseable.parse(x)
    }
  }

  final case class User(id: String, name: String)

  // Exercise: create Parse instance for User
  implicit val parseUser: Parse[User] = s => s.split("""\s+""") match {
    case Array(id, name) => Right(User(id, name))
    case _ => Left ("Invalid user string")
  }

  import ParseSyntax._
  println("lalala".parse[User])
  println("someId someName".parse[User])
}

object Task4 extends App {
  // Exercise: design a typesafe equals so i can do a === b, but it won't compile if a and b are of different types
  // define the typeclass (think of a method signature)
  // remember `a method b` is `a.method(b)`
  trait Equals[T] {
    def equals(e1: T, e2: T): Boolean
  }

  object EqualsSyntax {
    implicit class EqualsOps[T](e1: T) {
      def ===(e2: T)(implicit equatable: Equals[T]): Boolean = equatable.equals(e1, e2)
    }
  }

  final case class User(id: String, name: String)
  final case class SuperUser(id: String, name: String)

  implicit val userEquals: Equals[User] = (e1, e2) => e1.id == e2.id && e1.name == e2.name

  import EqualsSyntax._
  println(User("1", "Oleg") === User("1", "Oleg"))
  // println(User("1", "Oleg") === SuperUser("1", "Oleg")) // Will not compile (Type mismatch: Required: User; Found: SuperUser)
}

object AdvancedHomework extends App {
  // Exercise: create a typeclass for flatMap method
  // Considering there is a flatMap in Scala, I did implementation using F# naming - bind
  trait Bind[E[_]] {
    def bind[A, B](eT: E[A], f: A => E[B]): E[B]
  }

  object BindSyntax {
    implicit class BindOps[A, B, E[_]](eT: E[A]) {
      def bind(f: A => E[B])(implicit bindable: Bind[E]): E[B] = bindable.bind(eT, f)
    }
  }

  implicit val optionBind: Bind[Option] = new Bind[Option] {
    def bind[A, B](eT: Option[A], f: A => Option[B]): Option[B] = eT match {
      case Some(t) => f(t)
      case _ => None
    }
  }

  implicit val listBind: Bind[List] = new Bind[List] {
    def bind[A, B](eT: List[A], f: A => List[B]): List[B] = eT match {
      case head :: tail => f(head) ::: bind(tail, f)
      case _ => Nil
    }
  }

  import BindSyntax._

  def divide(x: Int, y: Int): Option[Int] = if (y == 0) None else Some(x / y)
  val someFive: Option[Int] = Some(5)
  println(someFive.bind { divide(10, _) })

  def duplicateAndToString[A](x: A): List[String] = List(x.toString, x.toString)
  val someList: List[Int] = (1 to 5).toList
  println(someList.bind(duplicateAndToString))
}

object TypeclassTask extends App {

  // Why am I not a Typeclass?
  // Exercise: Rework me so I am a typeclass
  trait HashCode[T] {
    def hash(entity: T): Int
  }

  // Exercise: Implement me a summoner
  object HashCode {
    def apply[F](implicit instance: HashCode[F]): HashCode[F] = instance
  }

  // Exercise: Implement syntax so I can "abc".hash
  object HashCodeSyntax {
    implicit class HashCodeOps[A](x: A) {
      def hash(implicit hashCode: HashCode[A]): Int = hashCode.hash(x)
    }
  }

  // Exercise: make an instance for String
  implicit val stringHash: HashCode[String] =
    _.zipWithIndex.map { case (c, i) => c * Math.pow(31, i) }.sum.toInt

  // Exercise: write "abc".hash to check everything
  import HashCodeSyntax._
  println("abc".hash)
}
