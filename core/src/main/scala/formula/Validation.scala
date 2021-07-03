package formula
//
import com.raquo.laminar.api.L._
//
//import scala.language.experimental.macros
//
sealed trait Validation[+E, +A] { self =>
  def value: A

  def zip[E1 >: E, B](that: Validation[E1, B]): Validation[E1, (A, B)] =
    (self, that) match {
      case (Validation.Succeed(a), Validation.Succeed(b)) =>
        Validation.Succeed((a, b))
      case (Validation.Warnings(ws, a), Validation.Warnings(ws2, b)) =>
        Validation.Warnings(::(ws.head, ws.tail ::: ws2), a -> b)
      case (Validation.Warnings(ws, a), that) =>
        Validation.Warnings(ws, a -> that.value)
      case (self, Validation.Warnings(ws2, b)) =>
        Validation.Warnings(ws2, self.value -> b)
    }

  def map[B](f: A => B): Validation[E, B] = self match {
    case Validation.Warnings(warnings, value) =>
      Validation.Warnings(warnings, f(value))
    case Validation.Succeed(value) =>
      Validation.Succeed(f(value))
  }

  def filterOrFail[E1 >: E](predicate: A => Boolean)(error: => E1): Validation[E1, A] =
    self match {
      case w @ Validation.Warnings(warnings, value) =>
        if (predicate(value)) w
        else Validation.Warnings(::(error, warnings), value)
      case succeed @ Validation.Succeed(value) =>
        if (predicate(value)) succeed
        else Validation.Warnings(::(error, Nil), value)
    }
}

object Validation {
  def apply[A](value: A): Validation[String, A] = Succeed(value)

  case class Warnings[+E, +A](warnings: ::[E], value: A) extends Validation[E, A]
  case class Succeed[+A](value: A)                       extends Validation[Nothing, A]
}
//
//trait Form[A] { self =>
//  def labelled(str: String): Form[A] = new Form[A] {
//    override def renderImpl(variable: Var[A])(implicit owner: Owner): Mod[HtmlElement] =
//      div(
//        cls("input-group"),
//        label(str),
//        self.renderImpl(variable)
//      )
//  }
//
//  def ~[B](that: Form[B]): Form[(A, B)] = new Form[(A, B)] {
//    override def renderImpl(variable: Var[(A, B)])(implicit owner: Owner): Mod[HtmlElement] =
//      Seq(
//        self.renderImpl(variable.zoom(_._1)(_ -> variable.now()._2)),
//        that.renderImpl(variable.zoom(_._2)(variable.now()._1 -> _))
//      )
//  }
//
//  def xmap[B](to: A => B)(from: B => A): Form[B] = new Form[B] {
//    override def renderImpl(variable: Var[B])(implicit owner: Owner): Mod[HtmlElement] =
//      self.renderImpl(variable.zoom[A](from)(to))
//  }
//
//  private[formula] def renderImpl(variable: Var[A])(implicit owner: Owner): Mod[HtmlElement]
//
//  def render(variable: Var[A]): FormElement =
//    form(
//      onMountInsert { ctx =>
//        div(renderImpl(variable)(ctx.owner))
//      }
//    )
//}
//
//object Form {
//  implicit val string: Form[String] = new Form[String] {
//    override def renderImpl(variable: Var[String])(implicit owner: Owner): HtmlElement =
//      input(
//        controlled(
//          value <-- variable,
//          onInput.mapToValue --> variable
//        )
//      )
//  }
//
//  // Var[A]
//  // Var[Validation[E, W, A]]
//  // Fail(E) | Warnings(Chunk[W], A) | Success(A)
//
//  /** - Warning
//    * Form.int.label("age").validate(_ > 10, "Must be older than 10")
//    *
//    * Form.string.label("name")
//    */
//  implicit val int: Form[Int] = string.xmap(_.toInt)(_.toString)
//
//  /** ZRef-esque approach
//    * failable setting and getting
//    */
//
//  def render[A](variable: Var[A])(implicit form: Form[A]): FormElement =
//    form.render(variable)
//}
