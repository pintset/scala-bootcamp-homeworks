package cats

import cats.Functor
import cats.data.NonEmptyList
import cats.kernel.Monoid
import cats.implicits._

trait MonoidHierarchy {

  /*
    Semigroup[A] {
      def combine(y: A): A
    }
   */

  /*
    Monoid[A] {
      def combine(y: A): A
    }

    def empty: A
   */

  // NonEmptyList

  type Error = String
  type Result[T] = Either[NonEmptyList[Error], T]

  // laws
  type A
  val anyValue: A = ???
  val v1: A = ???
  val v2: A = ???
  val v3: A = ???

  implicit val instance: Monoid[A]

  (Monoid[A].empty |+| anyValue) == anyValue // left identity

  (anyValue |+| Monoid[A].empty) == anyValue // right identity

  ((v1 |+| v2) |+| v3) == (v1 |+| (v2 |+| v3)) // associativity
}

object MonadHierarchy {
  /*
    Functor[F[A]] {
      def map(f: A => B): F[B]
    }
   */

  /*
    Applicative[F[A]] {
      def ap(f: F[A => B]): F[B]
    }

    def pure[A](x: A): F[A]
   */

  /*
    Monad[F[A]] {
      def flatMap(f: A => F[B]): F[B]
    }

    def pure[A](x: A): F[A]
   */

  // applicative: (F[A], F[B]) => F[(A, B)]
  // monad:        F[F[A]]     => F[A]
}

// Circular calls/dependencies allowed
object Excercises {
  trait Applicative[F[_]] extends Functor[F] {
    def map[A,B](fa: F[A])(f: A => B): F[B]

    def unit[A](a: => A): F[A]

    // implement methods using each other
    def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
      def tuple(a: A)(b: B): (A, B) = (a, b)
      val tupleF: F[A => B => (A, B)] = unit(tuple)
      val fab: F[(A, B)] = apply(apply(tupleF)(fa))(fb)
      map(fab) { x => f(x._1, x._2) }
    }

    def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] = map2(fab, fa) { (f, a) => f(a) }

    def sequence[A](fas: List[F[A]]): F[List[A]] = {
      val zero: F[List[A]] = unit(List.empty)
      fas.foldRight(zero) { (fa, fla) => map2(fa, fla) { (a, as) => a :: as } }
      // fas.foldRight(zero) { (fa, fla) => map2(fa, fla)(_ :: _)
    }

    def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] = sequence(as.map(f))
  }

  trait Monad[M[_]] extends Functor[M] {
    def unit[A](a: => A): M[A]

    // implement methods using each other
    def flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] = join(map(ma)(f))

    def join[A](mma: M[M[A]]): M[A] = flatMap(mma)(identity)

    def map[A,B](ma: M[A])(f: A => B): M[B] =
      flatMap(ma) { a => unit(f(a)) }

    def map2[A,B,C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] = {
      // join(map(ma) { a => map(mb) { b => f(a, b) } })
      flatMap(ma) { a => map(mb) { b => f(a, b) } }
    }
  }
}
