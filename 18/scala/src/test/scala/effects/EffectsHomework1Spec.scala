package effects

import EffectsHomework1._
import org.scalacheck.{Prop, Properties}
import org.scalacheck.Prop.forAll
import org.scalatest.EitherValues

import scala.concurrent.{Await, ExecutionContext}
import scala.util.{Failure, Success, Try}
import java.util.concurrent.LinkedTransferQueue

object EffectsHomework1Spec extends Properties("IO") with EitherValues {
  property("map") = forAll { (v: Int, f: Function[Int, Int]) =>
    IO(v).map(f).unsafeRunSync() == f(v)
  }

  property("map.identity") = forAll { (v: Int, f: Function[Int, Int]) =>
    IO(v).map(f).map(identity).unsafeRunSync() == IO(v).map(f).unsafeRunSync()
  }

  property("flatMap") = forAll { (v: Int, f: Function[Int, Int]) =>
    IO(v).flatMap(f.andThen(IO.pure)).unsafeRunSync() == f(v)
  }

  property("*>") = forAll { (v: Int, f1: Function[Int, Int], f2: Function[Int, Int]) =>
    val q = new LinkedTransferQueue[Int]()
    (IO(v).map(f1.andThen(q.put)) *> IO(q.poll)).map(f2).unsafeRunSync() == f2(f1(v))
  }

  property("as") = forAll { (v1: Int, v2: Int, f: Function[Int, Int]) =>
    val q = new LinkedTransferQueue[Int]()
    IO(v1).map(f.andThen(q.put)).as(v2 + q.poll).unsafeRunSync() == f(v1) + v2
  }

  property("attempt.success") = forAll { (v: Int, f: Function[Int, Int]) =>
    IO(v).map(f).attempt.unsafeRunSync() == Right(f(v))
  }

  property("attempt.failure.map") = forAll { (v: Int, t: Throwable, f: Function[Int, Int]) =>
    IO(v).map(f.andThen(_ => throw t)).attempt.unsafeRunSync().left.value == t
  }

  property("attempt.failure.flatMap") = forAll { (v: Int, t: Throwable, f: Function[Int, Int]) =>
    IO(v).flatMap(f.andThen(IO.pure)).map(_ => throw t).attempt.unsafeRunSync().left.value == t
  }

  property("option.success") = forAll { (v: Int, f: Function[Int, Int]) =>
    IO(v).map(f).option.unsafeRunSync().contains(f(v))
  }

  property("option.failure") = forAll { (v: Int, t: Throwable, f: Function[Int, Int]) =>
    IO(v).map(f.andThen(_ => throw t)).option.unsafeRunSync().isEmpty
  }

  property("handleErrorWith.success") = forAll { (v: Int, d: Int, f: Function[Int, Int]) =>
    IO(v).map(f).handleErrorWith(_ => IO(d)).unsafeRunSync() == f(v)
  }

  property("handleErrorWith.failure") = forAll { (v: Int, d: Int, t: Throwable, f: Function[Int, Int]) =>
    IO(v).map(f.andThen(_ => throw t)).handleErrorWith(_ => IO(d)).unsafeRunSync() == d
  }

  property("redeem.success") = forAll { (v: Int, d: Int, f: Function[Int, Int]) =>
    IO(v).map(f).redeem(_ => d, f).unsafeRunSync() == f(f(v))
  }

  property("redeem.failure") = forAll { (v: Int, d: Int, t: Throwable, f: Function[Int, Int]) =>
    IO(v).map(f.andThen(_ => throw t)).redeem(_ => d, f).unsafeRunSync() == d
  }

  property("redeemWith.success") = forAll { (v: Int, d: Int, f: Function[Int, Int]) =>
    IO(v).map(f).redeemWith(_ => IO.pure(d), f.andThen(IO.pure)).unsafeRunSync() == f(f(v))
  }

  property("redeemWith.failure") = forAll { (v: Int, d: Int, t: Throwable, f: Function[Int, Int]) =>
    IO(v).map(f.andThen(_ => throw t)).redeemWith(_ => IO.pure(d), f.andThen(IO.pure)).unsafeRunSync() == d
  }

  property("unsafeRunSync") = forAll { (v: Int) =>
    val q = new LinkedTransferQueue[Int]()
    IO(v).map(x => { q.put(x); x }).unsafeRunSync() == q.poll()
  }

  property("unsafeToFuture") = forAll { (v: Int, f: Function[Int, Int]) =>
    import scala.concurrent.duration._
    implicit val executionContext: ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global

    Await.result(IO(v).map(f).unsafeToFuture(), 1.second) == f(v)
  }

  property("stackUnsafe") = forAll { v: Int =>
    object TestApp extends App {
      val program: IO[Unit] = IO(v).flatMap(_ => program)
      program.unsafeRunSync()
    }

    Prop.throws(classOf[StackOverflowError]) {
      TestApp.main(Array.empty)
    }
  }

  property("suspend") = forAll { (v: Int, f: Function[Int, Int]) =>
    IO.suspend(IO.pure(f(v))).unsafeRunSync() == f(v)
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

  property("raiseError") = forAll { (e: Throwable) =>
    Prop.throws(e.getClass) {
      IO(throw e).unsafeRunSync()
    }
  }

  property("raiseUnless") = forAll { (cond: Boolean, t: Throwable) =>
    val result = IO.raiseUnless(cond)(t).attempt.unsafeRunSync()
    if (cond)
      result == Right(())
    else
      result.left.value == t
  }

  property("raiseWhen") = forAll { (cond: Boolean, t: Throwable) =>
    val result = IO.raiseWhen(cond)(t).attempt.unsafeRunSync()
    if (cond)
      result.left.value == t
    else
      result == Right(())
  }

  property("unlessA") = forAll { (v: Int, cond: Boolean) =>
    val q = new LinkedTransferQueue[Int]()
    IO.unlessA(cond)(IO({ q.add(v); () })).unsafeRunSync()

    if (cond) q.isEmpty
    else !q.isEmpty
  }

  property("whenA") = forAll { (v: Int, cond: Boolean) =>
    val q = new LinkedTransferQueue[Int]()
    IO.whenA(cond)(IO({ q.add(v); () })).unsafeRunSync()

    if (cond) !q.isEmpty
    else q.isEmpty
  }
}