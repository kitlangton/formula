package formula

import com.raquo.laminar.api.L._

trait ZVar[-A, +B] { self =>

  def set(a: A): Unit

  def get: B

  def signal: Signal[B]

  def update(f: B => A): Unit = set(f(get))

  def zip[A2, B2, A1 <: A](
    that: ZVar[A2, B2]
  ): ZVar[(A, A2), (B, B2)] =
    new ZVar[(A, A2), (B, B2)] {
      override def set(a: (A, A2)): Unit = {
        self.set(a._1)
        that.set(a._2)
      }

      override def get: (B, B2) =
        self.get -> that.get

      override def signal: Signal[(B, B2)] =
        self.signal.combineWith(that.signal)
    }

  def zipWith[A2, B2, A1 <: A, C](that: ZVar[A2, B2])(
    f: (B, B2) => C
  ): ZVar[(A, A2), C]                           =
    zip(that).map { case (b, b2) => f(b, b2) }

  /**
   * Transforms the `set` value of the `ZVar` wth the specified function.
   */
  final def contramap[C](f: C => A): ZVar[C, B] =
    dimap(f, identity)

  /**
   * Transforms both the `set` and `get` values of the `ZVar` with the
   * specified fallible functions.
   */
  final def dimap[C, D](f: C => A, g: B => D): ZVar[C, D] =
    new ZVar[C, D] {
      override def set(c: C): Unit = {
        val a = f(c)
        self.set(a)
      }

      override def get: D =
        g(self.get)

      override def signal: Signal[D] =
        self.signal.map(g)
    }

  /**
   * Transforms the `get` value of the `ZVar` with the specified function.
   */
  final def map[C](f: B => C): ZVar[A, C] =
    dimap(identity, f)

}

object ZVar {
  def make[A](value: A): ZVar[A, A] =
    new ZVar[A, A] {
      val laminarVar: Var[A] = Var(value)

      override def get: A = laminarVar.now()

      override def set(a: A): Unit = laminarVar.set(a)

      override def signal: Signal[A] = laminarVar.signal
    }
}
