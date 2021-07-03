package formula

import com.raquo.laminar.api.L._
import com.raquo.laminar.api.L
import formula.Form.FormVar

import scala.scalajs.js.timers.setTimeout
import scala.util.Try
import scala.util.matching.Regex

trait Form[A] { self =>

  private[formula] val variable: FormVar[A]

  def render: Mod[HtmlElement]

  def signal: Signal[Validation[String, A]] = variable.signal

  def set(value: A): Unit = variable.set(value)

  def validSignal: Signal[Option[A]] = variable.signal.map {
    case _: Validation.Warnings[_, _] => None
    case Validation.Succeed(value)    => Some(value)
  }

  def label(label: String): Form[A] = {
    val label0 = label

    new Form[A] {
      override val variable: FormVar[A] =
        self.variable

      override def render: Mod[HtmlElement] =
        Seq(
          L.label(label0),
          self.render
        )

    }
  }

  def validate(predicate: A => Boolean, error: => String): Form[A] =
    new Form[A] {
      override val variable: FormVar[A] =
        self.variable.map(_.filterOrFail(predicate)(error))

      override def render: Mod[HtmlElement] = {
        val touched = Var(false)

        val $isValid: Signal[Boolean] =
          variable.signal.combineWith(touched).map { case (b, touched) =>
            touched && !predicate(b.value)
          }

        div(
          variable.signalEither.changes.mapTo(true) --> touched,
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

  final def xmap[C](f: A => C)(g: C => A): Form[C] =
    new Form[C] {
      override val variable: FormVar[C]     = self.variable.dimap(g, _.map(f))
      override def render: Mod[HtmlElement] = self.render
    }

  def zip[B, A1 <: A](
      that: Form[B]
  ): Form[(A, B)] =
    new Form[(A, B)] {
      override val variable: FormVar[(A, B)] =
        self.variable.zipWith(that.variable)(_ zip _)

      override def render: Mod[HtmlElement] =
        Seq(self.render, that.render)
    }
}

object Form {
  private type ZFormVar[-A, +B] = ZVar[Nothing, Nothing, A, Validation[String, B]]
  type FormVar[A]               = ZFormVar[A, A]

  def succeed[A](value: => A): Form[A] = new Form[A] {
    override val variable: FormVar[A] = FormVar.make(value)

    override def render: Mod[HtmlElement] = new Modifier[HtmlElement] {
      override def apply(element: HtmlElement): Unit = ()
    }
  }

  object FormVar {
    def make[A](value: A): FormVar[A] = ZVar.make(value).map(Validation.Succeed(_))
  }

  implicit def string: Form[String] =
    new Form[String] {
      override val variable: FormVar[String] =
        FormVar.make("")

      override def render: Mod[HtmlElement] =
        div(
          input(
            placeholder("Name"),
            controlled(
              value <-- variable.signal.map(_.value),
              onInput.mapToValue --> { string => variable.set(string) }
            )
          )
        )
    }

  implicit def int: Form[Int] =
    new Form[Int] {
      override val variable: FormVar[Int] =
        FormVar.make(0)

      override def render: Mod[HtmlElement] =
        Forms.regex(variable, _.toIntOption, "[0-9]*".r)
    }

  implicit def double: Form[Double] =
    new Form[Double] {
      override val variable: FormVar[Double] =
        FormVar.make(0.0)

      val doubleRegex: Regex = "-?\\d*\\.?\\d*e?".r

      override def render: Mod[HtmlElement] =
        Forms.regex(variable, _.toDoubleOption, doubleRegex)
    }

  def dollars: Form[Double] =
    new Form[Double] {
      override val variable: FormVar[Double] =
        FormVar.make(0.0)

      val moneyRegex: Regex = "\\$? ?\\d{1,3}[,\\d{1,3}]*\\.?\\d{0,2}".r

      def renderMoney(string: String): String =
        "$ " + string.filter(c => c.isDigit || c == '.').takeWhile(_ != '.').reverse.grouped(3).mkString(",").reverse +
          string.dropWhile(_ != '.')

      override def render: Mod[HtmlElement] = {
        val stringVar     = Var("")
        val lastSelection = Var(0 -> 0)
        val lastKeyDown   = Var("")

        def parseDouble(string: String): Option[Double] =
          Option
            .when(moneyRegex.matches(string)) {
              string.filter(c => c.isDigit || c == '.').toDoubleOption
            }
            .flatten

        div(
          stringVar.signal.changes.map(parseDouble).collect { case Some(value) => value } --> { str =>
            variable.set(str)
          },
          input(
            variable.signal.map(s => renderMoney(s.value.toString)) --> stringVar,
            value <-- stringVar.signal,
            inContext { el =>
              onInput.mapToValue --> { string =>
                parseDouble(string) match {
                  case Some(_) =>
                    val rendered = renderMoney(string)
                    stringVar.set(rendered)
                    el.ref.value = stringVar.now()

                    val (start, end) = lastSelection.now()
                    val diff         = rendered.drop(start).takeWhile(_ != lastKeyDown.now().head).length + 1
                    el.ref.selectionStart = start + diff
                    el.ref.selectionEnd = end + diff
                  case None =>
                    el.ref.value = stringVar.now()

                    val (start, end) = lastSelection.now()
                    el.ref.selectionStart = start
                    el.ref.selectionEnd = end
                }
              }
            },
            inContext { el =>
              onKeyDown --> { ev =>
                lastKeyDown.set(ev.key)
                lastSelection.set(el.ref.selectionStart -> el.ref.selectionEnd)
              }
            }
          )
        )
      }
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
