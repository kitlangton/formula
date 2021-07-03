package formula

import com.raquo.laminar.api.L
import com.raquo.laminar.api.L._

private[formula] object EitherSyntax {
  implicit final class EitherOps[E, A](val self: Either[E, A]) extends AnyVal {
    def zip[E2 >: E, B](that: Either[E2, B])(implicit zippable: Zippable[A, B]): Either[E2, zippable.Out] =
      for {
        a <- self
        b <- that
      } yield zippable.zip(a, b)
  }
}

import formula.EitherSyntax._

sealed trait ZVar[+EA, +EB, -A, +B] { self =>

  def get: Either[EB, B]

  def set(a: A): Either[EA, Unit]

  def signalEither: Signal[Either[EB, B]]

  def zip[EA1 >: EA, EB1 >: EB, A2, B2, A1 <: A](
      that: ZVar[EA1, EB1, A2, B2]
  ): ZVar[EA1, EB1, (A, A2), (B, B2)] =
    new ZVar[EA1, EB1, (A, A2), (B, B2)] {
      override def set(a: (A, A2)): Either[EA1, Unit] =
        for {
          _ <- self.set(a._1)
          _ <- that.set(a._2)
        } yield ()

      override def get: Either[EB1, (B, B2)] =
        self.get zip that.get

      override def signalEither: Signal[Either[EB1, (B, B2)]] =
        self.signalEither.combineWithFn(that.signalEither)(_ zip _)
    }

  def fold[EC, ED, C, D](
      ea: EA => EC,
      eb: EB => ED,
      ca: C => Either[EC, A],
      bd: B => Either[ED, D]
  ): ZVar[EC, ED, C, D] =
    new ZVar[EC, ED, C, D] {
      override def set(c: C): Either[EC, Unit] =
        ca(c).flatMap { a =>
          self.set(a).left.map(ea)
        }

      override def get: Either[ED, D] =
        self.get.fold(e => Left(eb(e)), b => bd(b))

      override def signalEither: Signal[Either[ED, D]] =
        self.signalEither.map { ebb =>
          ebb.fold(e => Left(eb(e)), b => bd(b))
        }
    }

  def foldAll[EC, ED, C, D](
      ea: EA => EC,
      eb: EB => ED,
      ec: EB => EC,
      ca: C => B => Either[EC, A],
      bd: B => Either[ED, D]
  ): ZVar[EC, ED, C, D] = new ZVar[EC, ED, C, D] {

    override def set(c: C): Either[EC, Unit] =
      self.get.fold(
        e => Left(ec(e)),
        b => ca(c)(b).flatMap(a => self.set(a).left.map(ea))
      )

    override def get: Either[ED, D] =
      self.get.fold(e => Left(eb(e)), bd)

    override def signalEither: L.Signal[Either[ED, D]] =
      self.signalEither.map { ebb =>
        ebb.fold(e => Left(eb(e)), b => bd(b))
      }
  }

  /** Maps and filters the `get` value of the `ZVar` with the specified partial
    * function, returning a `ZVar` with a `get` value that succeeds with the
    * result of the partial function if it is defined or else fails with `None`.
    */
  final def collect[C](pf: PartialFunction[B, C]): ZVar[EA, Option[EB], A, C] =
    fold(identity, Some(_), Right(_), pf.lift(_).toRight(None))

  /** Transforms the `set` value of the `ZVar` wth the specified function.
    */
  final def contramap[C](f: C => A): ZVar[EA, EB, C, B] =
    contramapEither(c => Right(f(c)))

  /** Transforms the `set` value of the `ZVar` with the specified fallible
    * function.
    */
  final def contramapEither[EC >: EA, C](f: C => Either[EC, A]): ZVar[EC, EB, C, B] =
    dimapEither(f, Right(_))

  /** Transforms both the `set` and `get` values of the `ZVar` with the
    * specified functions.
    */
  final def dimap[C, D](f: C => A, g: B => D): ZVar[EA, EB, C, D] =
    dimapEither(c => Right(f(c)), b => Right(g(b)))

  /** Transforms both the `set` and `get` values of the `ZVar` with the
    * specified fallible functions.
    */
  final def dimapEither[EC >: EA, ED >: EB, C, D](f: C => Either[EC, A], g: B => Either[ED, D]): ZVar[EC, ED, C, D] =
    fold(identity, identity, f, g)

  /** Transforms both the `set` and `get` errors of the `ZVar` with the
    * specified functions.
    */
  final def dimapError[EC, ED](f: EA => EC, g: EB => ED): ZVar[EC, ED, A, B] =
    fold(f, g, Right(_), Right(_))

  /** Filters the `set` value of the `ZVar` with the specified predicate,
    * returning a `ZVar` with a `set` value that succeeds if the predicate is
    * satisfied or else fails with `None`.
    */
  final def filterInput[A1 <: A](f: A1 => Boolean): ZVar[Option[EA], EB, A1, B] =
    fold(Some(_), identity, a => if (f(a)) Right(a) else Left(None), Right(_))

  final def filterInputOrElse[A1 <: A, EA2](f: A1 => Boolean)(error: EA2)(implicit
      ev: EA <:< EA2
  ): ZVar[EA2, EB, A1, B] =
    filterInputWith(f)(_ => error)

  final def filterInputWith[A1 <: A, EA2](f: A1 => Boolean)(error: A1 => EA2)(implicit
      ev: EA <:< EA2
  ): ZVar[EA2, EB, A1, B] =
    fold(ev, identity, a => if (f(a)) Right(a) else Left(error(a)), Right(_))

  /** Filters the `get` value of the `ZVar` with the specified predicate,
    * returning a `ZVar` with a `get` value that succeeds if the predicate is
    * satisfied or else fails with `None`.
    */
  final def filterOutput(f: B => Boolean): ZVar[EA, Option[EB], A, B] =
    fold(identity, Some(_), Right(_), b => if (f(b)) Right(b) else Left(None))

  /** Transforms the `get` value of the `ZVar` with the specified function.
    */
  final def map[C](f: B => C): ZVar[EA, EB, A, C] =
    mapEither(b => Right(f(b)))

  /** Transforms the `get` value of the `ZVar` wth the specified fallible
    * function.
    */
  final def mapEither[EC >: EB, C](f: B => Either[EC, C]): ZVar[EA, EC, A, C] =
    dimapEither(Right(_), f)

  final def tapInputError(f: EA => Unit): ZVar[EA, EB, A, B] =
    dimapError(ea => { f(ea); ea }, identity)

  final def tapOutputError(f: EB => Unit): ZVar[EA, EB, A, B] =
    dimapError(identity, eb => { f(eb); eb })

}

object ZVar {
  implicit final class EVarOps[E, A](val self: ZVar[E, E, A, A])

  implicit final class UVarOps[E, A, B](val self: ZVar[E, Nothing, A, B]) {
    def signal: Signal[B] = self.signalEither.map(_.merge)
  }

  def make[A](value: A): ZVar[Nothing, Nothing, A, A] =
    new ZVar[Nothing, Nothing, A, A] {
      val laminarVar: Var[A] = Var(value)

      override def get: Either[Nothing, A] =
        Right(laminarVar.now())

      override def set(a: A): Either[Nothing, Unit] =
        Right(laminarVar.set(a))

      override def signalEither: Signal[Either[Nothing, A]] =
        laminarVar.signal.map(Right(_))
    }
}
