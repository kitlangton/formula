package formula

import magnolia.{CaseClass, Magnolia}

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros

case class FieldValidation[-A](predicate: A => Boolean, error: String) extends StaticAnnotation
case class FieldLabel(label: String)                                   extends StaticAnnotation

object DeriveForm {
  type Typeclass[A] = Form[A]

  def combine[A](caseClass: CaseClass[Form, A]): Form[A] = {
    val forms: List[Form[Any]] = caseClass.parameters.map { param =>
      val label = param.annotations
        .collectFirst { case label: FieldLabel => label.label }
        .getOrElse(param.label.capitalize)

      val form = param.typeclass
        .label(label)
        .asInstanceOf[Form[Any]]

      val validations = param.annotations.collect { //
        case v: FieldValidation[_] => v
      }

      val validatedForm = validations.foldLeft(form) { (acc, a) =>
        acc.validate(a.predicate.asInstanceOf[Any => Boolean], a.error)
      }

      validatedForm
    }.toList

    forms
      .foldRight(Form.succeed(List.empty[Any])) { (acc, frm) =>
        acc
          .zip(frm)
          .xmap { case (h, tail) =>
            h :: tail
          } {
            case Nil       => (Nil, Nil)
            case h :: tail => (h, tail)
          }
      }
      .xmap { params =>
        caseClass.rawConstruct(params)
      } { a =>
        caseClass.parameters.map(_.dereference(a)).toList
      }
  }

  implicit def gen[A]: Form[A] = macro Magnolia.gen[A]
}
