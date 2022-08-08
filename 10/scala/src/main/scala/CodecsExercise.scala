object CodecsExercise extends App {
  // Exercise 6
  trait Functor[F[_]] {
    def fmap[A, B](fa: F[A])(f: A => B): F[B]
  }

  implicit class FunctorOps[F[_], A](fa: F[A])(implicit functor: Functor[F]) {
    def fmap[B](f: A => B): F[B] = implicitly[Functor[F]].fmap(fa)(f)
  }

  object Functor {
    def apply[F[_]](implicit f: Functor[F]): Functor[F] = f
  }

  // Exercise 8
  trait Contravariant[F[_]] {
    def contramap[A, B](fa: F[A])(f: B => A): F[B]
  }

  implicit class ContravariantOps[F[_], A](fa: F[A])(implicit c: Contravariant[F]) {
    def contramap[B](f: B => A): F[B] = c.contramap(fa)(f)
  }

  object Contravariant {
    def apply[F[_]](implicit c: Contravariant[F]): Contravariant[F] = c
  }

  sealed trait Json {
    def /(key: String): JsonResult = this match {
      case JsonObject(value) => JsonResult(value.get(key))
      case _ => JsonResult(None)
    }
  }

  final case object JsonNull extends Json
  final case class JsonString(value: String) extends Json
  final case class JsonInt(value: Int) extends Json
  final case class JsonArray(value: List[Json]) extends Json
  final case class JsonObject(value: Map[String, Json]) extends Json

  final case class JsonResult(v: Option[Json]) {
    def /(key: String): JsonResult = ???

    def as[A: Decoder]: Option[A] =
      v.flatMap(Decoder[A].fromJson)
  }

  // Encoder
  trait Encoder[A] {
    def toJson(a: A): Json
  }

  object Encoder {
    def apply[A: Encoder]: Encoder[A] = implicitly[Encoder[A]]
  }

  implicit class EncoderOps[A: Encoder](a: A) {
    def toJson: Json = Encoder[A].toJson(a)
  }

  // Exercise 9
  implicit val encoderContravariant: Contravariant[Encoder] = new Contravariant[Encoder] {
    def contramap[A, B](fa: Encoder[A])(f: B => A): Encoder[B] = b => fa.toJson(f(b))
  }

  // Decoder
  trait Decoder[A] {
    def fromJson(json: Json): Option[A]
  }

  object Decoder {
    def apply[A: Decoder]: Decoder[A] = implicitly[Decoder[A]]
  }

  implicit class DecoderOps(json: Json) {
    def as[A: Decoder]: Option[A] = Decoder[A].fromJson(json)
  }

  // Exercise 7
  implicit val decoderFunctor: Functor[Decoder] = new Functor[Decoder] {
    def fmap[A, B](fa: Decoder[A])(f: A => B): Decoder[B] =
      fa.fromJson(_).map(f)
  }

  // Exercise 1. Implement Encoder and Decoder for Int.
  implicit val IntEncoder: Encoder[Int] = JsonInt(_)
  implicit val IntDecoder: Decoder[Int] = json => Some(json).collect { case JsonInt(i) => i }

  100.toJson
  JsonNull.as[Int]

  // Exercise 2. Implement Encoder and Decoder for String.
  implicit val StringEncoder: Encoder[String] = JsonString(_)
  implicit val StringDecoder: Decoder[String] = {
    case JsonString(s) => Some(s)
    case _ => None
  }

  "Example".toJson
  JsonNull.as[String]

  final case class Person(name: String, age: Int)

  // Exercise 3. Implement Encoder and Decoder for Person.
  implicit val PersonEncoder: Encoder[Person] =
    p => JsonObject(Map("name" -> JsonString(p.name), "age" -> JsonInt(p.age)))

  //  implicit val PersonDecoder: Decoder[Person] = {
  //    case JsonObject(map) =>
  //      for {
  //        name <- map.get("name").flatMap(_.as[String])
  //        age <- map.get("age").flatMap(_.as[Int])
  //      } yield Person(name, age)
  //
  //    case _ => None
  //  }

  implicit val PersonDecoder: Decoder[Person] = value =>
    for {
      name <- (value / "name").as[String]
      age <- (value / "age").as[Int]
    } yield Person(name, age)

  Person("Ivan", 25).toJson
  JsonNull.as[Person]

  // Exercise 4. Implement Encoder and Decoder for List with any content.
  implicit def listEncoder[A: Encoder]: Encoder[List[A]] = xs => JsonArray(xs.map(Encoder[A].toJson(_)))
  implicit def listDecoder[A: Decoder]: Decoder[List[A]] = {
    case JsonArray(xs) =>
      xs.foldRight(Option.empty[List[A]]) { (json, state) => state.flatMap(xs => json.as[A].map(_ :: xs)) }
    case _ => None
  }

  final case class EntityId(id: String) extends AnyVal

  // Exercise 5. Implement Encoder and Decoder for EntityId with any content.
  // implicit val idEncoder: Encoder[EntityId] = _.id.toJson
  implicit val idEncoder: Encoder[EntityId] = Encoder[String].contramap(_.id)

  // implicit val idDecoder: Decoder[EntityId] = _.as[String].map(EntityId)
  implicit val idDecoder: Decoder[EntityId] = Decoder[String].fmap(EntityId)

  val eId = EntityId("7").toJson
  eId.as[EntityId]
  eId.as[String]

  // Exercise 6. Describe Functor
  // 1. Typeclass itself: `trait Functor`
  // 2. Typeclass Summoner: `object Functor`
  // 3. Typeclass Ops: `implicit class FunctorOps`

  // Exercise 7. Implement Functor for decoder: `implicit val decoderFunctor`

  // Exercise 8. Describe Contravariant
  // 1. Typeclass itself: `trait Contravariant`
  // 2. Typeclass Summoner: `object Contravariant`
  // 3. Typeclass Ops: `implicit class ContravariantOps`

  // Exercise 9. Implement Contravariant for encoder: `implicit val encoderContravariant`

  // Functions Example
  val foo1: String => Int = _.length
  val foo2: Boolean => String = if (_) "100" else "1"

  // Exercise 10. Implement Functor and Contravariant for functions:
  implicit def functionFunctor[A]: Functor[Function[A, *]] = new Functor[A => *] {
    def fmap[B, C](fa: A => B)(f: B => C): A => C = a => f(fa(a))
  }
  implicit def functionContravariant[C]: Contravariant[Function[*, C]] = new Contravariant[* => C] {
    def contramap[A, B](fa: A => C)(f: B => A): B => C = b => fa(f(b))
  }

  val foo3: Boolean => Int = functionFunctor.fmap(foo2)(foo1)
  val foo4: Boolean => Int = functionContravariant.contramap(foo1)(foo2)
}
