package formula

import com.raquo.laminar.api.L
import com.raquo.laminar.api.L._
import formula.Form.FormValidation

import scala.util.matching.Regex

sealed trait Form[A] { self =>
  final def label(label: String): Form[A] = Form.Labeled(self, label)

  final def validate(predicate: A => Boolean, error: => String): Form[A] =
    self match {
      case Form.ValidatedForm(form, validations) =>
        Form.ValidatedForm(form, ::(FormValidation(predicate, error), validations))
      case _                                     =>
        Form.ValidatedForm(self, ::(FormValidation(predicate, error), Nil))
    }

  final def xmap[B](f: A => B)(g: B => A): Form[B] = Form.XMap(self, f, g)

  final def zip[B, A1 <: A](that: Form[B]): Form[(A, B)] = Form.Zip(self, that)

  final def placeholder(placeholder: String): Form[A] =
    self match {
      case Form.ValidatedForm(form, validations) =>
        Form.ValidatedForm(form.placeholder(placeholder), validations)
      case Form.XMap(form, f, g)                 =>
        Form.XMap(form.placeholder(placeholder), f, g)
      case Form.Labeled(form, label)             =>
        Form.Labeled(form.placeholder(placeholder), label)
      case Form.Input("", className, formValue)  =>
        Form.Input(placeholder, className, formValue)
      case other                                 => other
    }
}

object Form {
  type FormVar[A] = ZVar[Nothing, Nothing, A, Validation[String, A]]

  case class FormValue[A](variable: FormVar[A], node: Mod[HtmlElement]) {
    def signal: Signal[Validation[String, A]] =
      variable.signal

    def set(value: A): Unit = variable.set(value)
  }

  def render[A](form: Form[A]): FormValue[A] = form match {
    case ValidatedForm(form, validations) =>
      val FormValue(var0, node0) = render(form)
      val var1                   = var0.map {
        validations.foldLeft(_) { (acc, v) =>
          acc.filterOrFail(v.predicate)(v.error)
        }
      }
      val touched                = Var(false)

      val $warnings: Signal[List[String]] =
        var1.signal.combineWith(touched).map {
          case (v, true) => v.warnings
          case _         => List.empty
        }

      FormValue[A](
        var1,
        div(
          var1.signal.changes.mapTo(true) --> touched,
          cls.toggle("formula-invalid") <-- $warnings.map(_.nonEmpty),
          node0,
          children <-- $warnings.map {
            _.map(str => div(cls("formula-validation-error"), str))
          }
        )
      )

    case Zip(left, right) =>
      val FormValue(lVar, lNode) = render(left)
      val FormValue(rVar, rNode) = render(right)
      val zipVar                 = lVar.zipWith(rVar)(_ zip _)
      FormValue(zipVar, Seq(lNode, rNode))

    case XMap(form, f, g) =>
      val FormValue(var0, node0) = render(form)
      FormValue(var0.dimap(g, _.map(f)), node0)

    case Labeled(form, label) =>
      val FormValue(var0, node0) = render(form)
      FormValue(var0, Seq(L.label(label), node0))

    case Succeed(value) =>
      FormValue(FormVar.make(value), span())

    case Input(placeholder, className, makeFormValue) =>
      makeFormValue(InputConfig(placeholder, className))

  }

  final case class FormValidation[A](predicate: A => Boolean, error: String)
  final case class ValidatedForm[A](form: Form[A], validations: ::[FormValidation[A]]) extends Form[A]
  final case class Zip[A, B](left: Form[A], right: Form[B])                            extends Form[(A, B)]
  final case class XMap[A, B](form: Form[A], f: A => B, g: B => A)                     extends Form[B]
  final case class Labeled[A](form: Form[A], label: String)                            extends Form[A]
  final case class Succeed[A](value: A)                                                extends Form[A]
  final case class Input[A](placeholder: String, className: String, formValue: InputConfig => FormValue[A])
      extends Form[A]

  object Input {
    def make[A](f: InputConfig => FormValue[A]): Form[A] =
      Input("", "", f)
  }

  case class InputConfig(placeholder: String, className: String) {
    def modifiers: Mod[HtmlElement] = Seq(
      L.placeholder(placeholder),
      L.className(className)
    )
  }

  def succeed[A](value: A): Form[A] = Form.Succeed(value)

  object FormVar {
    def make[A](value: A): FormVar[A] = ZVar.make(value).map(Validation.Succeed(_))
  }

  implicit val string: Form[String] = Form.Input.make[String] { config =>
    val var0 = FormVar.make("")
    val node = div(
      input(
        config.modifiers,
        controlled(
          value <-- var0.signal.map(_.value),
          onInput.mapToValue --> { string => var0.set(string) }
        )
      )
    )

    FormValue(var0, node)
  }

  implicit val int: Form[Int] = {
    val intRegex = "[0-9]*".r
    Form.Input.make { config =>
      val var0 = FormVar.make(0)
      val node = Fields.regex(config, var0, _.toIntOption, intRegex)
      FormValue(var0, node)
    }
  }

  implicit val double: Form[Double] = {
    val doubleRegex: Regex = "-?\\d*\\.?\\d*e?".r
    Form.Input.make { config =>
      val var0 = FormVar.make(0.0)
      val node = Fields.regex(config, var0, _.toDoubleOption, doubleRegex)
      FormValue(var0, node)
    }
  }

  val dollars: Form[Double] = Form.Input.make(Fields.makeDollars)
}
