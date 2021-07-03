package formula

sealed trait Validation[+E, +A] { self =>
  def value: A

  def zip[E1 >: E, B](that: Validation[E1, B]): Validation[E1, (A, B)] =
    (self, that) match {
      case (Validation.Succeed(a), Validation.Succeed(b))            =>
        Validation.Succeed((a, b))
      case (Validation.Warnings(ws, a), Validation.Warnings(ws2, b)) =>
        Validation.Warnings(::(ws.head, ws.tail ::: ws2), a -> b)
      case (Validation.Warnings(ws, a), that)                        =>
        Validation.Warnings(ws, a -> that.value)
      case (self, Validation.Warnings(ws2, b))                       =>
        Validation.Warnings(ws2, self.value -> b)
    }

  def map[B](f: A => B): Validation[E, B] = self match {
    case Validation.Warnings(warnings, value) =>
      Validation.Warnings(warnings, f(value))
    case Validation.Succeed(value)            =>
      Validation.Succeed(f(value))
  }

  def filterOrFail[E1 >: E](predicate: A => Boolean)(error: => E1): Validation[E1, A] =
    self match {
      case w @ Validation.Warnings(warnings, value) =>
        if (predicate(value)) w
        else Validation.Warnings(::(error, warnings), value)
      case succeed @ Validation.Succeed(value)      =>
        if (predicate(value)) succeed
        else Validation.Warnings(::(error, Nil), value)
    }
}

object Validation {
  def apply[A](value: A): Validation[String, A] = Succeed(value)

  case class Warnings[+E, +A](warnings: ::[E], value: A) extends Validation[E, A]
  case class Succeed[+A](value: A)                       extends Validation[Nothing, A]
}
