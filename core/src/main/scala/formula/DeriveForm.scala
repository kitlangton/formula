package formula

import formula.ZForm.Form
import magnolia.{CaseClass, Magnolia}

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros

case class FieldValidation[-A](predicate: A => Boolean, error: String) extends StaticAnnotation
case class FieldLabel(label: String)                                   extends StaticAnnotation

object DeriveForm {
  type Typeclass[A] = Form[A]

//  private def zoomToParam(variable: Var[A], param: Param[Typeclass, A])(implicit owner: Owner): Var[param.PType] =
//    variable.zoom[param.PType](a => param.dereference(a))(value =>
//      caseClass.construct { p =>
//        if (p == param) value
//        else p.dereference(variable.now())
//      }
//    )
//
//  override def renderImpl(variable: Var[A])(implicit owner: Owner): Mod[HtmlElement] =
//    caseClass.parameters.map { param =>
//      val paramVar = zoomToParam(variable, param)
//      param.typeclass.label(param.label).render
//    }.toList

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
      .foldRight(ZForm.succeed(List.empty[Any])) { (acc, frm) =>
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

  //    new ZForm[A, A] {
//
//      override private[formula] def zVar: ZFormVar[A, A] = {
//        val vars: List[ZFormVar[Any, Any]] =
//          caseClass.parameters.map(_.typeclass.zVar.asInstanceOf[ZFormVar[Any, Any]]).toList
//
//        val list: ZVar[Nothing, Nothing, List[Any], Validation[String, List[Any]]] =
//          vars.foldRight(ZVar.make(List.empty[Any]).map(Validation(_))) { (v, acc) =>
//            acc
//              .zip(v)
//              .dimap[List[Any], Validation[String, List[Any]]](
//                { case head :: tail =>
//                  (tail, head)
//                },
//                { case (acc, v) =>
//                  acc.zip(v).map { case (acc, v) => acc.prepended(v) }
//                }
//              )
//          }
//
//        list.dimap[A, Validation[String, A]](
//          { a =>
//            caseClass.parameters.map { param => param.dereference(a) }.toList
//          },
//          _.map(list => caseClass.rawConstruct(list))
//        )
//      }
//
//      override def render: Mod[HtmlElement] =
//        caseClass.parameters.map { param =>
//          param.typeclass.label(param.label.capitalize).render
//        }
//    }

  implicit def gen[A]: Form[A] = macro Magnolia.gen[A]
}
