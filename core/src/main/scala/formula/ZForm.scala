package formula

import com.raquo.laminar.api.L._
import com.raquo.laminar.api.L
import formula.ZForm.{FormVar, ZFormVar}

import scala.util.Try
import scala.util.matching.Regex

trait ZForm[-A, +B] { self =>
  def set(value: A): Unit = zVar.set(value)

  private[formula] def zVar: ZFormVar[A, B]
  def render: Mod[HtmlElement]

  def signal: Signal[Validation[String, B]] = zVar.signal

  def validSignal: Signal[Option[B]] = zVar.signal.map {
    case _: Validation.Warnings[_, _] => None
    case Validation.Succeed(value)    => Some(value)
  }

  def label(label: String): ZForm[A, B] = {
    val label0 = label

    new ZForm[A, B] {
      override def zVar: ZVar[Nothing, Nothing, A, Validation[String, B]] =
        self.zVar

      override def render: Mod[HtmlElement] =
        Seq(
          L.label(label0),
          self.render
        )

    }
  }

  def validate(predicate: B => Boolean, error: => String): ZForm[A, B] =
    new ZForm[A, B] {
      override def zVar: ZVar[Nothing, Nothing, A, Validation[String, B]] =
        self.zVar.map(_.filterOrFail(predicate)(error))

      override def render: Mod[HtmlElement] = {
        val touched = Var(false)

        val $isValid: Signal[Boolean] =
          zVar.signal.combineWith(touched).map { case (b, touched) =>
            touched && !predicate(b.value)
          }

        div(
          zVar.signalEither.changes.mapTo(true) --> touched,
          cls.toggle("formula-invalid") <-- $isValid,
          self.render,
          child.maybe <-- $isValid.map { isValid =>
            Option.when(isValid) {
              div(
                cls("formula-validation-error"),
                error
              )
            }
          }
        )
      }

    }

  final def xmap[C](f: B => C)(g: C => A): ZForm[C, C] =
    new ZForm[C, C] {
      override def zVar: ZFormVar[C, C]     = self.zVar.dimap(g, _.map(f))
      override def render: Mod[HtmlElement] = self.render
    }

  final def dimap[C, D](f: C => A, g: B => D): ZForm[C, D] =
    new ZForm[C, D] {
      override def zVar: ZFormVar[C, D]     = self.zVar.dimap(f, _.map(g))
      override def render: Mod[HtmlElement] = self.render
    }

  def map[C](f: B => C): ZForm[A, C] = new ZForm[A, C] {
    override def zVar: ZFormVar[A, C] = self.zVar.map(_.map(f))

    override def render: Mod[HtmlElement] =
      self.render
  }

  def zip[A2, B2, A1 <: A](
      that: ZForm[A2, B2]
  ): ZForm[(A, A2), (B, B2)] =
    new ZForm[(A, A2), (B, B2)] {
      override def zVar: ZFormVar[(A, A2), (B, B2)] =
        self.zVar.zip(that.zVar).map { case (va, vb) => va zip vb }

      override def render: Mod[HtmlElement] =
        Seq(
          self.render,
          that.render
        )
    }
}

object ZForm {
  type Form[A]          = ZForm[A, A]
  type ZFormVar[-A, +B] = ZVar[Nothing, Nothing, A, Validation[String, B]]
  type FormVar[A]       = ZFormVar[A, A]

  def succeed[A](value: => A): ZForm[A, A] = new ZForm[A, A] {
    override private[formula] def zVar = FormVar.make(value)

    override def render: Mod[HtmlElement] = new Modifier[HtmlElement] {
      override def apply(element: HtmlElement): Unit = ()
    }
  }

  object FormVar {
    def make[A](value: A): FormVar[A] = ZVar.make(value).map(Validation.Succeed(_))
  }

  implicit def string: ZForm[String, String] =
    new ZForm[String, String] {
      override val zVar: FormVar[String] =
        FormVar.make("")

      override def render: Mod[HtmlElement] =
        div(
          input(
            placeholder("Name"),
            controlled(
              value <-- zVar.signal.map(_.value),
              onInput.mapToValue --> { string => zVar.set(string) }
            )
          )
        )
    }

  implicit def int: ZForm[Int, Int] =
    new ZForm[Int, Int] {
      override val zVar: FormVar[Int] =
        FormVar.make(0)

      override def render: Mod[HtmlElement] =
        Forms.regex(zVar, _.toIntOption, "[0-9]*".r)
    }

  implicit def double: Form[Double] =
    new ZForm[Double, Double] {
      override val zVar: FormVar[Double] =
        FormVar.make(0.0)

      val doubleRegex: Regex = "-?\\d*\\.?\\d*e?".r

      override def render: Mod[HtmlElement] =
        Forms.regex(zVar, _.toDoubleOption, doubleRegex)
    }

}

object Forms {
  def regex[A](variable: FormVar[A], parser: String => Option[A], regex: Regex): HtmlElement = {
    val stringVar = Var("")

    input(
      stringVar.signal.changes.map(parser).collect { case Some(d) => d } --> { value => variable.set(value) },
      variable.signal.changes.map(_.value.toString) --> stringVar,
      inContext { el =>
        controlled(
          value <-- stringVar,
          onInput.mapToValue --> { value =>
            if (regex.matches(value)) {
              stringVar.set(value)
            } else {
              val start = Try(el.ref.selectionStart).getOrElse(0)
              el.ref.value = stringVar.now()
              el.ref.selectionStart = start - 1
              el.ref.selectionEnd = start - 1
            }
          }
        )
      }
    )
  }

}
