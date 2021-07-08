package formula

import com.raquo.laminar.api.L
import com.raquo.laminar.api.L._
import formula.Form.{FormValidation, FormVar}

import scala.util.Try
import scala.util.matching.Regex

sealed trait Form[A] { self =>
  def build: FormValue[A] = Form.build(self)

  def widen[A1 >: A]: Form[A1] = self.asInstanceOf[Form[A1]]

  final def help(helpText: String): Form[A] = self match {
    case Form.ValidatedForm(form, validations)                                 =>
      Form.ValidatedForm(form.help(helpText), validations)
    case Form.Default(form, defaultValue)                                      =>
      Form.Default(form.help(helpText), defaultValue)
    case Form.XMap(form, f, g)                                                 =>
      Form.XMap(form.help(helpText), f, g)
    case Form.Input(label, placeholder, className, "", validations, formValue) =>
      Form.Input(label, placeholder, className, helpText, validations, formValue)
    case other                                                                 => other
  }

  final def label(label: String): Form[A] = self match {
    case Form.ValidatedForm(form, validations)                                   =>
      Form.ValidatedForm(form.label(label), validations)
    case Form.Default(form, defaultValue)                                        =>
      Form.Default(form.label(label), defaultValue)
    case Form.XMap(form, f, g)                                                   =>
      Form.XMap(form.label(label), f, g)
    case Form.XFlatMap(form, f, g)                                               =>
      Form.XFlatMap(form.label(label), f, g)
    case Form.Input(_, placeholder, className, helpText, validations, formValue) =>
      Form.Input(label, placeholder, className, helpText, validations, formValue)
    case other                                                                   => other
  }
//    Form.Labeled(self, label)

  final private def many: Form[List[A]] = Form.Many(self)

  final def default(value: Option[A]): Form[A] =
    value.map(Form.Default(self, _)).getOrElse(self)

  final def validate(predicate: A => Boolean, error: => String): Form[A] =
    self match {
      case Form.ValidatedForm(form, validations)                                       =>
        Form.ValidatedForm(form.validate(predicate, error), validations)
      case Form.XMap(form, f, g)                                                       =>
        Form.XMap(form.validate(predicate.asInstanceOf[Any => Boolean], error), f, g)
      case Form.Labeled(form, label)                                                   =>
        Form.Labeled(form.validate(predicate, error), label)
      case Form.Default(form, defaultValue)                                            =>
        Form.Default(form.validate(predicate, error), defaultValue)
      case Form.Input(label, placeholder, className, helpText, validations, formValue) =>
        Form.Input(label, placeholder, className, helpText, FormValidation(predicate, error) :: validations, formValue)
      case other                                                                       =>
        other
    }
//    self match {
//      case Form.ValidatedForm(form, validations) =>
//        Form.ValidatedForm(form, ::(FormValidation(predicate, error), validations))
//      case _                                     =>
//        Form.ValidatedForm(self, ::(FormValidation(predicate, error), Nil))
//    }

  final def xmap[B](f: A => B)(g: B => A): Form[B]           = Form.XMap(self, f, g)
  final def xflatMap[B](f: A => Form[B])(g: B => A): Form[B] = Form.XFlatMap(self, f, g)

  final def zip[B, A1 <: A](that: Form[B]): Form[(A, B)]       = Form.Zip(self, that)
  final def flatZip[B, A1 <: A](f: A => Form[B]): Form[(A, B)] = Form.FlatZip(self, f)

  final def placeholder(placeholder: String): Form[A] =
    self match {
      case Form.ValidatedForm(form, validations)                              =>
        Form.ValidatedForm(form.placeholder(placeholder), validations)
      case Form.XMap(form, f, g)                                              =>
        Form.XMap(form.placeholder(placeholder), f, g)
      case Form.Labeled(form, label)                                          =>
        Form.Labeled(form.placeholder(placeholder), label)
      case Form.Default(form, defaultValue)                                   =>
        Form.Default(form.placeholder(placeholder), defaultValue)
      case Form.Input(label, "", className, helpText, validations, formValue) =>
        Form.Input(label, placeholder, className, helpText, validations, formValue)
      case other                                                              => other
    }
}

object Form {
  type FormVar[A] = ZVar[A, Validation[String, A]]

  def build[A](form: Form[A]): FormValue[A] = form match {
    case ValidatedForm(form, validations) =>
      val FormValue(var0, node0) = build(form)

      val var1 = var0.map {
        validations.foldLeft(_) { (acc, v) =>
          acc.filterOrFail(v.predicate)(v.error)
        }
      }

      val touched = Var(false)

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
            _.map(str => div(cls("invalid-feedback"), str))
          }
        )
      )

    case Default(form, defaultValue) =>
      val formValue = build(form)
      formValue.set(defaultValue)
      formValue

    case Many(form) =>
      val FormValue(var0, node0) = build(form)
      val countForm              = build(Form.int.label("Count"))

      val variables = Var(List.empty[ZVar[A, Validation[String, A]]])

      // This is so very ugly.
      val variable = new ZVar[List[A], Validation[String, List[A]]] {
        override def get: Validation[String, List[A]] =
          variables.now().foldLeft(Validation(List.empty[A])) { (acc, v) =>
            acc.zip(v.get).map { case (acc, v) => acc.appended(v) }
          }

        override def set(a: List[A]): Unit =
          a.zip(variables.now()).foreach { case (a, v) =>
            v.set(a)
          }

        override def signal: L.Signal[Validation[String, List[A]]] =
          variables.signal.flatMap {
            _.foldLeft[L.Signal[Validation[String, List[A]]]](Val(Validation(List.empty[A]))) { (acc, v) =>
              acc.combineWithFn(v.signal) { case (l, r) =>
                l.zip(r).map { case (as, a) => as.appended(a) }
              }
            }
          }

      }

      countForm.variable.set(0)
      val node = Seq(
        countForm.node,
        div(children <-- countForm.signal.map(v => (0 until v.value).toList).split(identity) { (id, _, _) =>
          val formValue = build(form)
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
      val FormValue(lVar, lNode) = build(left)
      val FormValue(rVar, rNode) = build(right)
      val zipVar                 = lVar.zipWith(rVar)(_ zip _)
      FormValue(zipVar, Seq(lNode, rNode))

    case FlatZip(form, f) =>
      val FormValue(varA, node0) = build(form)

      val value          = varA.get.value
      val firstForm      = f(value)
      val firstFormValue = build(firstForm)
      val memoizedForms  = collection.mutable.Map(firstForm -> firstFormValue)
      val bFormVar       = Var(firstFormValue)

      def updateSubform(value: Any): Unit = {
        val form      = f(value)
        val formValue = memoizedForms.getOrElse(
          form, {
            println(s"CREATING NEW FORM VALUE FOR $form")
            val formValue = Form.build(form)
            memoizedForms.addOne((form, formValue))
            formValue
          }
        )
        bFormVar.set(formValue)
      }

      val varB = new ZVar[(Any, Any), Validation[String, (Any, Any)]] {
        override def set(ab: (Any, Any)): Unit = {
          varA.set(ab._1)
          bFormVar.now().variable.set(ab._2)
        }

        override def get: Validation[String, (Any, Any)] =
          varA.get zip bFormVar.now().variable.get

        override def signal: Signal[Validation[String, (Any, Any)]] =
          bFormVar.signal.flatMap { bForm =>
            varA.signal.combineWithFn(bForm.variable.signal)(_ zip _)
          }
      }

      val node =
        Seq(
          node0,
          varA.signal --> { va => updateSubform(va.value) },
          child <-- bFormVar.signal.map(form => div(form.node))
        )

      FormValue(varB, node)

    case XMap(form, f, g) =>
      val FormValue(var0, node0) = build(form)
      FormValue(var0.dimap(g, _.map(f)), node0)

    case XFlatMap(form, f, g) =>
      val FormValue(varA, node0) = build(form)

      val value         = varA.get.value
      val firstForm     = Form.build(f(value))
      val memoizedForms = collection.mutable.Map(value -> firstForm)
      val formVar       = Var(firstForm)

      def updateSubform(value: Any): Unit = {
        val form = memoizedForms.getOrElse(
          value, {
            val form = Form.build(f(value))
            memoizedForms.addOne((value, form))
            form
          }
        )
        formVar.set(form)
      }

      val varB = new ZVar[A, Validation[String, A]] {
        override def set(b: A): Unit = {
          varA.set(g(b))
          formVar.now().variable.set(b)
        }

        override def get: Validation[String, A] =
          formVar.now().variable.get

        override def signal: Signal[Validation[String, A]] =
          formVar.signal.flatMap(_.variable.signal)
      }

      val node =
        Seq(
          node0,
          varA.signal --> { va => updateSubform(va.value) },
          child <-- formVar.signal.map(form => div(form.node))
        )

      FormValue(varB, node)

    case Labeled(form, label) =>
      val FormValue(var0, node0) = build(form)
      FormValue(var0, Seq(L.label(label), node0))

    case Succeed(value) =>
      FormValue(FormVar.make(value), span())

    case Input(label, placeholder, className, helpText, validations, makeFormValue) =>
      val config = InputConfig(
        label = label,
        placeholder = placeholder,
        helpText = helpText,
        validations = validations,
        className = className
      )

      val FormValue(var0, node0) = makeFormValue(config)

      val node =
        div(
          cls("mb-3"),
          L.label(cls("form-label"), label),
          node0,
          children <-- var0.signal.map {
            _.warnings.map(str => div(cls("invalid-feedback"), str))
          },
          Option.when(helpText.nonEmpty)(
            div(
              cls("form-text"),
              helpText
            )
          )
        )

      FormValue(var0, node)

  }

  final case class FormValidation[A](predicate: A => Boolean, error: String)
  final case class ValidatedForm[A](form: Form[A], validations: ::[FormValidation[A]]) extends Form[A]
  final case class Default[A](form: Form[A], defaultValue: A)                          extends Form[A]
  final case class Zip[A, B](left: Form[A], right: Form[B])                            extends Form[(A, B)]
  final case class Many[A](form: Form[A])                                              extends Form[List[A]]
  final case class XMap[A, B](form: Form[A], f: A => B, g: B => A)                     extends Form[B]
  final case class XFlatMap[A, B](form: Form[A], f: A => Form[B], g: B => A)           extends Form[B]
  final case class FlatZip[A, B](form: Form[A], f: A => Form[B])                       extends Form[(A, B)]
  final case class Labeled[A](form: Form[A], label: String)                            extends Form[A]
  final case class Succeed[A](value: A)                                                extends Form[A]
  final case class Input[A](
    label: String,
    placeholder: String,
    className: String,
    helpText: String,
    validations: List[FormValidation[A]],
    formValue: InputConfig[A] => FormValue[A]
  )                                                                                    extends Form[A]

  object Input {
    def make[A](f: InputConfig[A] => FormValue[A]): Form[A] =
      Input("", "", "form-control", "", List.empty, f)
  }

  case class InputConfig[A](
    label: String,
    placeholder: String,
    className: String,
    helpText: String,
    validations: List[FormValidation[A]],
    inputType: String = "text"
  ) {
    def wrap(el: Mod[HtmlElement]): Mod[HtmlElement] =
      div(
        cls("mb-3"),
        L.label(cls("form-label"), label),
        el,
        Option.when(helpText.nonEmpty)(
          div(
            cls("form-text"),
            helpText
          )
        )
      )

    def validate(variable: FormVar[A]): FormVar[A] =
      variable.map {
        validations.foldLeft(_) { (acc, v) =>
          acc.filterOrFail(v.predicate)(v.error)
        }
      }

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

  def select[A](source: Source[List[A]])(renderOption: A => String): Form[Option[A]] = Form.Input.make[Option[A]] {
    config =>
      val signal = source.toObservable.toSignalIfStream(_.toSignal(List.empty))
      val var0   = FormVar.make(Option.empty[A])
      val node   = L.select(
        config.modifiers,
        composeEvents(onInput.mapToValue)(_.withCurrentValueOf(signal)) --> Observer[(String, List[A])] {
          case (idxString, options) =>
            var0.set(Try(options(idxString.toInt)).toOption)
        },
        inContext { el =>
          var0.signal.map(_.value).withCurrentValueOf(signal) --> Observer[(Option[A], List[A])] {
            {
              case (Some(a), options) =>
                val idx = options.indexOf(a)
                if (idx >= 0) {
                  el.ref.value = idx.toString
                } else {
                  el.ref.value = ""
                }

              case (None, _) =>
                el.ref.value = ""
            }
          }
        },
        option(
          value("")
        ),
        children <-- signal.map(_.zipWithIndex).split(identity) { case ((opt, idx), _, _) =>
          option(
            value(idx.toString),
            renderOption(opt)
          )
        }
      )
      FormValue(var0, node)
  }

  implicit val string: Form[String] = Form.Input.make[String] { config =>
    val var0    = config.validate(FormVar.make(""))
    val touched = Var(false)

    val node =
      input(
        cls.toggle("is-invalid") <-- var0.signal.combineWithFn(touched.signal) {
          _.warnings.nonEmpty && _
        },
        config.modifiers,
        onInput.mapTo(true) --> touched,
        controlled(
          value <-- var0.signal.map(_.value),
          onInput.mapToValue --> { string => var0.set(string) }
        )
      )

    FormValue(var0, node)
  }

  implicit val boolean: Form[Boolean] =
    Form.Input.make { config =>
      val var0    = config.validate(FormVar.make(false))
      val touched = Var(false)
      val node    =
        input(
          display("block"),
          cls("form-check-input"),
          cls.toggle("is-invalid") <-- var0.signal.combineWithFn(touched.signal) {
            _.warnings.nonEmpty && _
          },
          config.copy(className = "").modifiers,
          `type`("checkbox"),
          controlled(
            checked <-- var0.signal.map(_.value),
            onClick.mapToChecked --> { bool =>
              touched.set(true)
              var0.set(bool)
            }
          )
        )
      FormValue(var0, node)
    }

  implicit val int: Form[Int] = {
    val intRegex = "[0-9]*".r
    Form.Input.make { config =>
      val var0 = config.validate(FormVar.make(0))
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

case class FormValue[A](variable: FormVar[A], node: Mod[HtmlElement]) {
  def signal: Signal[Validation[String, A]] =
    variable.signal

  def set(value: A): Unit = variable.set(value)

  def update(f: A => A): Unit = variable.set(variable.get.map(f).value)

  lazy val $value = signal.map(_.value)
}
