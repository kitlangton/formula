package formula

import com.raquo.laminar.api.L
import com.raquo.laminar.api.L._
import formula.Form.FormValidation

import scala.util.matching.Regex

sealed trait Form[A] { self =>
  def widen[A1 >: A]: Form[A1] = self.asInstanceOf[Form[A1]]

  final def label(label: String): Form[A] = Form.Labeled(self, label)

  final private def many: Form[List[A]] = Form.Many(self)

  final def default(value: Option[A]): Form[A] =
    value.map(Form.Default(self, _)).getOrElse(self)

  final def validate(predicate: A => Boolean, error: => String): Form[A] =
    self match {
      case Form.ValidatedForm(form, validations) =>
        Form.ValidatedForm(form, ::(FormValidation(predicate, error), validations))
      case _                                     =>
        Form.ValidatedForm(self, ::(FormValidation(predicate, error), Nil))
    }

  final def xmap[B](f: A => B)(g: B => A): Form[B]           = Form.XMap(self, f, g)
  final def xflatMap[B](f: A => Form[B])(g: B => A): Form[B] = Form.XFlatMap(self, f, g)

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

    lazy val $value = signal.map(_.value)
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

    case Default(form, defaultValue) =>
      val formValue = render(form)
      formValue.set(defaultValue)
      formValue

    case Many(form) =>
      val FormValue(var0, node0) = render(form)
      val countForm              = render(Form.int.label("Count"))

      val variables = Var(List.empty[ZVar[Nothing, Nothing, A, Validation[String, A]]])

      // This is so very ugly.
      val variable = new ZVar[Nothing, Nothing, List[A], Validation[String, List[A]]] {
        override def get: Either[Nothing, Validation[String, List[A]]] =
          Right(variables.now().foldLeft(Validation(List.empty[A])) { (acc, v) =>
            acc.zip(v.get.right.get).map { case (acc, v) => acc.appended(v) }
          })

        override def set(a: List[A]): Either[Nothing, Unit] = {
          a.zip(variables.now()).map { case (a, v) =>
            v.set(a)
          }
          Right(())
        }

        override def signalEither: L.Signal[Either[Nothing, Validation[String, List[A]]]] =
          variables.signal.flatMap {
            _.foldLeft[L.Signal[Either[Nothing, Validation[String, List[A]]]]](Val(Right(Validation(List.empty[A])))) {
              (acc, v) =>
                acc.combineWithFn(v.signalEither) { case (Right(l), Right(r)) =>
                  Right(l.zip(r).map { case (as, a) => as.appended(a) })
                }
            }
          }

      }

      countForm.variable.set(0)
      val node = Seq(
        countForm.node,
        div(children <-- countForm.signal.map(v => (0 until v.value).toList).split(identity) { (id, _, _) =>
          val formValue = render(form)
          if (variables.now().length > id + 1)
            variables.update(_.updated(id, formValue.variable.asInstanceOf[FormVar[A]]))
          else
            variables.update(_.appended(formValue.variable.asInstanceOf[FormVar[A]]))
          div(formValue.node)
        }),
        // Prune extra variables
        countForm.signal.map(_.value) --> { count =>
          if (variables.now().length > count) variables.update(_.take(count))
        }
      )
      FormValue(variable.asInstanceOf[FormVar[A]], node)

    case Zip(left, right) =>
      val FormValue(lVar, lNode) = render(left)
      val FormValue(rVar, rNode) = render(right)
      val zipVar                 = lVar.zipWith(rVar)(_ zip _)
      FormValue(zipVar, Seq(lNode, rNode))

    case XMap(form, f, g) =>
      val FormValue(var0, node0) = render(form)
      FormValue(var0.dimap(g, _.map(f)), node0)

    case XFlatMap(form, f, g) =>
      val FormValue(varA, node0) = render(form)

      val value     = varA.get.toOption.get.value
      val firstForm = Form.render(f(value))

      val memoizedForms = collection.mutable.Map(value -> firstForm)
      val formVar       = Var(firstForm)

      def updateSubform(value: Any): Unit = {
        val form = memoizedForms.getOrElse(
          value, {
            val form = Form.render(f(value))
            memoizedForms.addOne((value, form))
            form
          }
        )
        formVar.set(form)
      }

      val varB = new ZVar[Nothing, Nothing, A, Validation[String, A]] {
        override def set(b: A): Either[Nothing, Unit] = {
          varA.set(g(b))
          formVar.now().variable.set(b)
        }

        override def get: Either[Nothing, Validation[String, A]] =
          formVar.now().variable.get

        override def signalEither: Signal[Either[Nothing, Validation[String, A]]] =
          formVar.signal.flatMap(_.variable.signalEither)
      }

      val node =
        Seq(
          node0,
          varA.signal --> { va => updateSubform(va.value) },
          child <-- formVar.signal.map(form => div(form.node))
        )

      FormValue(varB, node)

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
  final case class Default[A](form: Form[A], defaultValue: A)                          extends Form[A]
  final case class Zip[A, B](left: Form[A], right: Form[B])                            extends Form[(A, B)]
  final case class Many[A](form: Form[A])                                              extends Form[List[A]]
  final case class XMap[A, B](form: Form[A], f: A => B, g: B => A)                     extends Form[B]
  final case class XFlatMap[A, B](form: Form[A], f: A => Form[B], g: B => A)           extends Form[B]
  final case class Labeled[A](form: Form[A], label: String)                            extends Form[A]
  final case class Succeed[A](value: A)                                                extends Form[A]
  final case class Input[A](placeholder: String, className: String, formValue: InputConfig => FormValue[A])
      extends Form[A]

  object Input {
    def make[A](f: InputConfig => FormValue[A]): Form[A] =
      Input("", "", f)
  }

  case class InputConfig(placeholder: String, className: String, inputType: String = "text") {
    def modifiers: Mod[HtmlElement] = Seq(
      L.placeholder(placeholder),
      L.className(className),
      L.`type`(inputType)
    )
  }

  def succeed[A](value: A): Form[A] = Form.Succeed(value)

  object FormVar {
    def make[A](value: A): FormVar[A] = ZVar.make(value).map(Validation.Succeed(_))
  }

  def select[A](options: Seq[A])(renderOption: A => String): Form[A] = Form.Input.make[A] { config =>
    val var0 = FormVar.make(options.head)
    val node = L.select(
      config.modifiers,
      onInput.mapToValue --> { idxString =>
        var0.set(options(idxString.toInt))
      },
      inContext { el =>
        var0.signal --> { a =>
          val idx = options.indexOf(a.value)
          if (idx >= 0) {
            el.ref.value = idx.toString
          }
        }
      },
      options.zipWithIndex.map { case (opt, idx) =>
        option(
          value(idx.toString),
          renderOption(opt)
        )
      }
    )
    FormValue(var0, node)
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

  implicit val boolean: Form[Boolean] =
    Form.Input.make { config =>
      val var0 = FormVar.make(false)
      val node = input(
        config.modifiers,
        `type`("checkbox"),
        controlled(
          checked <-- var0.signal.map(_.value),
          onClick.mapToChecked --> { bool => var0.set(bool) }
        )
      )
      FormValue(var0, node)
    }

  implicit val int: Form[Int] = {
    val intRegex = "[0-9]*".r
    Form.Input.make { config =>
      val var0 = FormVar.make(0)
      val node = Fields.regex(config.copy(inputType = "number"), var0, _.toIntOption, intRegex)
      FormValue(var0, node)
    }
  }

  implicit val double: Form[Double] = {
    val doubleRegex: Regex = "-?\\d*\\.?\\d*e?".r
    Form.Input.make { config =>
      val var0 = FormVar.make(0.0)
      val node = Fields.regex(config.copy(inputType = "number"), var0, _.toDoubleOption, doubleRegex)
      FormValue(var0, node)
    }
  }

  val dollars: Form[Double] = Form.Input.make(Fields.makeDollars)

}
