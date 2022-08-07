object QAndAExamples extends App {

  // 1. Semigroup
  // 1.1. Implement all parts of the typeclass definition
  trait Semigroup[A] {
    def combine(x: A, y: A): A
  }

  object Semigroup {
    def apply[A](implicit s: Semigroup[A]): Semigroup[A] = s

    import syntax._

    def combineAll[A: Semigroup](list: List[A]): A =
      list.reduceLeft(_ combine _)

    def combineAll[A: Semigroup](list: List[A], startingElement: A): A =
      list.foldLeft(startingElement)(_ combine _)

    object syntax {
      implicit class SemigroupOps[A: Semigroup](x: A) {
        def combine(y: A): A = Semigroup[A].combine(x, y)
      }
    }

    //    object instances {
    //      implicit val intSemigroup: Semigroup[Int] = (x, y) => x + y
    //      implicit val longSemigroup: Semigroup[Long] = (x, y) => x + y
    //      implicit val stringSemigroup: Semigroup[String] = (x, y) => x ++ y
    //    }
  }

  // 1.2. Implement Semigroup for Long, String
  import Monoid.instances._

  // 1.3. Implement combineAll(list: List[A]) for non-empty lists
  import Semigroup._

  combineAll(List(1, 2, 3)) == 6

  // 1.4. Implement combineAll(list: List[A], startingElement: A) for all lists

  combineAll(List(1, 2, 3), 0) == 6
  combineAll(List(), 1) == 1

  // 2. Monoid
  // 2.1. Implement Monoid which provides `empty` value (like startingElement in previous example) and extends Semigroup
  trait Monoid[A] extends Semigroup[A] {
    def empty: A
  }

  object Monoid {
    def apply[A](implicit m: Monoid[A]): Monoid[A] = m

    object instances {
      implicit val intMonoid: Monoid[Int] = new Monoid[Int] {
        def empty: Int = 0
        def combine(x: Int, y: Int): Int = x + y
      }

      implicit val longMonoid: Monoid[Long] = new Monoid[Long] {
        def empty: Long = 0
        def combine(x: Long, y: Long): Long = x + y
      }

      implicit val stringMonoid: Monoid[String] = new Monoid[String] {
        def empty: String = ""
        def combine(x: String, y: String): String = x ++ y
      }

      import Semigroup.syntax._

      implicit def optionMonoid[A: Semigroup]: Monoid[Option[A]] = new Monoid[Option[A]] {
        def empty: Option[A] = None
        def combine(xO: Option[A], yO: Option[A]): Option[A] = (xO, yO) match {
          case (Some(x), Some(y)) => Some(x combine y)
          case (x, y) => x.orElse(y)
        }
      }

      implicit def function1Monoid[T1, R: Monoid]: Monoid[T1 => R] = new Monoid[T1 => R] {
        def empty: T1 => R = _ => Monoid[R].empty
        def combine(x: T1 => R, y: T1 => R): T1 => R =
          t => x(t) combine y(t)
      }

      implicit def listMonoid[A: Monoid]: Monoid[List[A]] = new Monoid[List[A]] {
        def empty: List[A] = List.empty[A]
        def combine(x: List[A], y: List[A]): List[A] = x ++ y
      }
    }
  }

  // 2.2. Implement Monoid for Long, String

  // 2.3. Implement combineAll(list: List[A]) for all lists

  combineAll(List(1, 2, 3)) == 6

  // 2.4. Implement Monoid for Option[A]

  combineAll(List(Some(1), None, Some(3))) == Some(4)

  combineAll[Option[Int]](List(None, None)) == None
  combineAll(List[Option[Int]](None, None)) == None
  combineAll(List(Option.empty[Int], Option.empty)) == None

  // combineAll[Option[Int]](List()) == None
  // combineAll(List.empty[Option[Int]]) == None

  // 2.5. Implement Monoid for Function1 (for result of the function)

  combineAll(List((a: String) => a.length, (a: String) => a.toInt)) // === (a: String) => (a.length + a.toInt)
  combineAll(List((a: String) => a.length, (a: String) => a.toInt)).apply("123") /// === 126

  // 3. Functor
  trait Functor[F[_]] {
    def fmap[A, B](fa: F[A])(f: A => B): F[B]
  }

  implicit class FunctorOps[F[_]: Functor, A](fa: F[A]) {
    def fmap[B](f: A => B): F[B] = Functor[F].fmap(fa)(f)
  }

  object Functor {
    def apply[F[_]: Functor]: Functor[F] = implicitly[Functor[F]]
  }

  implicit val optionFunctor: Functor[Option] = new Functor[Option] {
    override def fmap[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  }

  // 4. Semigroupal
  // 4.1. Implement Semigroupal which provides `product[A, B](fa: F[A], fb: F[B]): F[(A, B)]`,
  // so in combination with Functor we'll be able to call for example `plus` on two Options (its content)
  trait Semigroupal[F[_]] extends Functor[F] {
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  }

  object Semigroupal {
    def apply[F[_]: Semigroupal]: Semigroupal[F] = implicitly

    object instances {
      implicit val optionSemigroupal: Semigroupal[Option] = new Semigroupal[Option] {
        def product[A, B](fa: Option[A], fb: Option[B]): Option[(A, B)] = (fa, fb) match {
          case (Some(a), Some(b)) => Some(a, b)
          case _ => None
        }
        def fmap[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
      }

      implicit def mapSemigroupal[K]: Semigroupal[Map[K, *]] = new Semigroupal[Map[K, *]] {
        def product[A, B](fa: Map[K, A], fb: Map[K, B]): Map[K, (A, B)] =
          (fa.keySet intersect fb.keySet).map(k => (k, (fa(k), fb(k)))).toMap

        def fmap[A, B](fa: Map[K, A])(f: A => B): Map[K, B] = fa.map { case (k, a) => (k, f(a)) }
      }
    }

    implicit class SemigroupalOps[F[_]: Semigroupal, A](fa: F[A]) {
      def product[B](fb: F[B]): F[(A, B)] = Semigroupal[F].product(fa, fb)
    }

    implicit class Tuple2Ops[F[_]: Semigroupal, A, B](tuple: (F[A], F[B])) {
      def mapN[R](f: (A, B) => R): F[R] =
      //(tuple._1 product tuple._2).fmap { case (a, b) => f(a, b) }
        (tuple._1 product tuple._2).fmap(f.tupled)
    }
  }

  // 4.2. Implement Semigroupal for Option
  import Semigroupal.instances._

  // 4.3. Implement `mapN[R](f: (A, B) => R): F[R]` extension method for Tuple2[F[A], F[B]]
  import Semigroupal._
  Option(1) product Option(2)

  (Option(1), Option(2)).mapN(_ + _) == Some(3)
  (Option(1), Option("2")).mapN(_ + _) // Option[String]
  (Option(1), Option("2")).mapN(_ + _.length)
  (Option(1), Option.empty[Int]).mapN(_ + _)      == None

  // 4.4. Implement Semigroupal for Map

  (Map(1 -> "a", 2 -> "b"), Map(2 -> "c")).mapN(_ + _) == Map(2 -> "bc")

  // 5. Applicative
  trait Applicative[F[_]] extends Semigroupal[F] with Functor[F] {
    def pure[A](x: A): F[A]
  }

  object Applicative {
    def apply[F[_]: Applicative]: Applicative[F] = implicitly

    def traverse[F[_]: Applicative, A, B](as: List[A])(f: A => F[B]): F[List[B]] =
      as.foldRight(Applicative[F].pure(List.empty[B])) { (a, fbs) =>
        val fb = f(a)
        Applicative[F].product(fb, fbs).fmap { case (b, bs) => b :: bs }
      }

    object syntax {
      implicit class ApplicativeOps[A](x: A) {
        def pure[F[_]: Applicative]: F[A] = Applicative[F].pure(x)
      }
    }

    object instances {
      implicit val eitherStringAppplicative: Applicative[Either[String, *]] = new Applicative[Either[String, *]] {
        override def pure[A](x: A): Either[String, A] = Right(x)
        override def product[A, B](fa: Either[String, A], fb: Either[String, B]): Either[String, (A, B)] = (fa, fb) match {
          case (Right(a), Right(b)) => Right((a, b))
          case (Left(a), Left(b)) => Left(a ++ b)
          case (Left(a), Right(_)) => Left(a)
          case (Right(_), Left(b)) => Left(b)
        }
        override def fmap[A, B](fa: Either[String, A])(f: A => B): Either[String, B] = fa.map(f)
      }

      implicit val optionApplicative: Applicative[Option] = new Applicative[Option] {
        def pure[A](x: A): Option[A] = Some(x)
        def product[A, B](fa: Option[A], fb: Option[B]): Option[(A, B)] = (fa, fb) match {
          case (Some(a), Some(b)) => Some((a, b))
          case _ => None
        }
        def fmap[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
      }

      implicit val listApplicative: Applicative[List] = new Applicative[List] {
        def pure[A](x: A): List[A] = List(x)
        def product[A, B](fa: List[A], fb: List[B]): List[(A, B)] =
          for {
            a <- fa
            b <- fb
          }  yield (a, b)

        def fmap[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
      }
    }
  }

  // 5.1. Implement Applicative for Option, Either
  import Applicative.instances._

  // 5.2. Implement `traverse` for all Applicatives instead of Option
  // def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = ???
  import Applicative._

  traverse(List(1, 2, 3)) { i =>
    Option.when(i % 2 == 1)(i)
  } == None

  traverse[Option, Int, Int](List(1, 2, 3)) { i =>
    Some(i + 1)
  } == Some(List(2, 3, 4))

  // 6. Foldable
  // 6.1. Implement Foldable with `foldLeft`
  trait Foldable[F[_]] {
    def foldLeft[A, B](as: F[A])(zero: B)(folder: (B, A) => B): B
  }

  object Foldable {
    def apply[F[_]: Foldable]: Foldable[F] = implicitly

    import Semigroup.syntax._
    import Applicative.syntax._
    import syntax._

    def traverse[G[_]: Applicative, F[_]: Foldable: Applicative, A, B](as: F[A])(f: A => G[B])(implicit m: Monoid[F[B]]): G[F[B]] = {
      as.foldLeft(m.empty.pure[G]) { (gfbs, a) =>
        val gb = f(a)
        (gfbs product gb).fmap { case (fbs, b) =>
          val fb = b.pure[F]
          fbs combine fb
        }
      }
    }

    object syntax {
      implicit class FoldableOps[F[_]: Foldable, A](fa: F[A]) {
        def foldLeft[B](zero: B)(folder: (B, A) => B): B = Foldable[F].foldLeft(fa)(zero)(folder)
        def traverse[G[_]: Applicative, B](f: A => G[B])(implicit m: Monoid[F[B]], a: Applicative[F]): G[F[B]] =
          Foldable.traverse(fa)(f)
      }
    }

    object instances {
      implicit val listFoldable: Foldable[List] = new Foldable[List] {
        def foldLeft[A, B](as: List[A])(zero: B)(folder: (B, A) => B): B = as.foldLeft(zero)(folder)
      }
    }
  }

  // 6.2. Implement Foldable for List
  // Note: we can use here foldLeft from standard library

  // 6.3. Implement `traverse` for all Foldables instead of List
  import Foldable.instances._
  import Foldable.syntax._

  List(1, 2, 3).traverse { i => Option(i + 1) } == Some(List(2, 3, 4))
}
