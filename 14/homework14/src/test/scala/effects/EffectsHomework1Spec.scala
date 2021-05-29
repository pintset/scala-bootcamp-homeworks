package effects

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalatest.EitherValues
import scala.concurrent.{Await, ExecutionContext}
import scala.util.{Success, Failure, Try}
import java.util.concurrent.LinkedTransferQueue

import EffectsHomework1._

object EffectsHomework1Spec extends Properties("IO") with EitherValues {
  property("map") = forAll { (v: Int, f: Function[Int, Int]) =>
    IO.delay(v).map(f).unsafeRunSync() == f(v)
  }

  property("map.identity") = forAll { (v: Int, f: Function[Int, Int]) =>
    IO.delay(v).map(f).map(identity).unsafeRunSync() == IO.delay(v).map(f).unsafeRunSync()
  }

  property("flatMap") = forAll { (v: Int, f: Function[Int, Int]) =>
    IO.delay(v).flatMap(f.andThen(IO.pure)).unsafeRunSync() == f(v)
  }

  property("*>") = forAll { (v: Int, f1: Function[Int, Int], f2: Function[Int, Int]) =>
    val q = new LinkedTransferQueue[Int]()
    (IO.delay(v).map(f1.andThen(q.put)) *> IO.delay(q.poll)).map(f2).unsafeRunSync() == f2(f1(v))
  }

  property("as") = forAll { (v1: Int, v2: Int, f: Function[Int, Int]) =>
    val q = new LinkedTransferQueue[Int]()
    IO.delay(v1).map(f.andThen(q.put)).as(v2 + q.poll).unsafeRunSync() == f(v1) + v2
  }

  property("attempt.success") = forAll { (v: Int, f: Function[Int, Int]) =>
    IO.delay(v).map(f).attempt.unsafeRunSync().right.value == f(v)
  }

  property("attempt.failure.map") = forAll { (v: Int, t: Throwable, f: Function[Int, Int]) =>
    IO.delay(v).map(f.andThen(_ => throw t)).attempt.unsafeRunSync().left.value == t
  }

  property("attempt.failure.flatMap") = forAll { (v: Int, t: Throwable, f: Function[Int, Int]) =>
    IO.delay(v).flatMap(f.andThen(IO.pure)).map(_ => throw t).attempt.unsafeRunSync().left.value == t
  }

  property("option") = forAll { (v: Int, f: Function[Int, Int]) =>
    IO.delay(v).map(f).option.unsafeRunSync().contains(f(v))
  }

  property("handleErrorWith.success") = forAll { (v: Int, d: Int, f: Function[Int, Int]) =>
    IO.delay(v).map(f).handleErrorWith(_ => IO(d)).unsafeRunSync() == f(v)
  }

  property("handleErrorWith.failure") = forAll { (v: Int, d: Int, t: Throwable, f: Function[Int, Int]) =>
    IO.delay(v).map(f.andThen(_ => throw t)).handleErrorWith(_ => IO(d)).unsafeRunSync() == d
  }

  property("redeem.success") = forAll { (v: Int, d: Int, f: Function[Int, Int]) =>
    IO.delay(v).map(f).redeem(_ => d, f).unsafeRunSync() == f(f(v))
  }

  property("redeem.failure") = forAll { (v: Int, d: Int, t: Throwable, f: Function[Int, Int]) =>
    IO.delay(v).map(f.andThen(_ => throw t)).redeem(_ => d, f).unsafeRunSync() == d
  }

  property("redeemWith.success") = forAll { (v: Int, d: Int, f: Function[Int, Int]) =>
    IO.delay(v).map(f).redeemWith(_ => IO.pure(d), f.andThen(IO.pure)).unsafeRunSync() == f(f(v))
  }

  property("redeemWith.failure") = forAll { (v: Int, d: Int, t: Throwable, f: Function[Int, Int]) =>
    IO.delay(v).map(f.andThen(_ => throw t)).redeemWith(_ => IO.pure(d), f.andThen(IO.pure)).unsafeRunSync() == d
  }

  property("unsafeRunSync") = forAll { (v: Int, f: Function[Int, Int]) =>
    import scala.concurrent.duration._
    implicit val executionContext: ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global

    Await.result(IO.delay(v).map(f).unsafeToFuture(), 1.second) == f(v)
  }

  property("fromEither") = forAll { e: Either[Throwable, Int] =>
    val io = IO.fromEither(e)
    e match {
      case Left(_) => io.attempt.unsafeRunSync().left.value == e.left.value
      case Right(v) => io.unsafeRunSync() ==  v
    }
  }

  property("fromOption") = forAll { (o: Option[Int], t: Throwable) =>
    val io = IO.fromOption(o)(t)
    o match {
      case None => io.attempt.unsafeRunSync().left.value == t
      case Some(v) => io.unsafeRunSync() == v
    }
  }

  property("fromTry") = forAll { t: Try[Int] =>
    val io = IO.fromTry(t)
    t match {
      case Success(v) => io.unsafeRunSync() == v
      case Failure(e) => io.attempt.unsafeRunSync().left.value == e
    }
  }

  property("raiseWhen") = forAll { (cond: Boolean, t: Throwable) =>
    val io = IO.raiseWhen(cond)(t).attempt
    if (cond)
      io.unsafeRunSync().left.value == t
    else
      io.unsafeRunSync().right.value == ()
  }

  property("whenA") = forAll { (v: Int, cond: Boolean) =>
    val q = new LinkedTransferQueue[Int]()
    val actionIO = IO({ q.add(v); () })
    val ioIsUnit = IO.whenA(cond)(actionIO).unsafeRunSync() == ()

    if (cond)
      ioIsUnit && !q.isEmpty
    else
      ioIsUnit && q.isEmpty
  }
}
