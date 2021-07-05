package formula

import com.raquo.laminar.api.L._
import magnolia.{CaseClass, Magnolia, SealedTrait, Subtype}

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros

case class FieldValidation[-A](predicate: A => Boolean, error: String) extends StaticAnnotation
case class FieldLabel(label: String)                                   extends StaticAnnotation

object DeriveForm {
  type Typeclass[A] = Form[A]

  def dispatch[T](sealedTrait: SealedTrait[Typeclass, T]): Typeclass[T] = {
    val options: Map[String, Subtype[Typeclass, T]] =
      sealedTrait.subtypes.map { sub =>
        getSubtypeLabel(sub) -> sub
      }.toMap

    Form
      .select(options.keys.toList)(identity)
      .xflatMap { label =>
        options(label).typeclass.widen[T]
      } { subtype =>
        sealedTrait.dispatch(subtype)(getSubtypeLabel)
      }
  }

  def combine[A](caseClass: CaseClass[Form, A]): Form[A] = {
    val forms: List[Form[Any]] = caseClass.parameters.map { param =>
      val label = param.annotations.collectFirst { case label: FieldLabel => label.label }
        .getOrElse(titleCase(param.label))

      val form = param.typeclass
        .label(label)
        .placeholder(label)
        .default(param.default)
        .asInstanceOf[Form[Any]]

      // Add validations
      val validations   = param.annotations.collect { case v: FieldValidation[_] => v }
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

  def build[A](implicit form: Form[A]): Form.FormValue[A] = Form.render(form)

  private def getSubtypeLabel[T](sub: Subtype[Typeclass, T]): String =
    sub.annotations.collectFirst { case label: FieldLabel => label.label }.getOrElse(titleCase(sub.typeName.short))

  /**
   * someParameterName -> Some Parameter Name
   * camelCase -> Title Case
   */
  private def titleCase(string: String): String                      =
    string.split("(?=[A-Z])").map(_.capitalize).mkString(" ")
}
