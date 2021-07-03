package formula

import com.raquo.laminar.api.L._
import formula.ZForm.{Form, ZFormVar}
import magnolia.{CaseClass, Magnolia, Param}

import scala.language.experimental.macros

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

  def combine[A](caseClass: CaseClass[Form, A]): Form[A] =
    new ZForm[A, A] {
      override private[formula] def zVar: ZFormVar[A, A] = {
        val vars: List[ZFormVar[Any, Any]] =
          caseClass.parameters.map(_.typeclass.zVar.asInstanceOf[ZFormVar[Any, Any]]).toList

        val list: ZVar[Nothing, Nothing, List[Any], List[Any]] =
          vars.foldRight(ZVar.make(List.empty[Any])) { (v, acc) =>
            acc
              .zip(v)
              .dimap[List[Any], List[Any]](
                { case head :: tail =>
                  (tail, head)
                },
                { case (acc, v) =>
                  acc.prepended(v.value)
                }
              )
          }

        list.dimap[A, Validation.Succeed[A]](
          { a =>
            caseClass.parameters.map { param => param.dereference(a) }.toList
          },
          { list => Validation.Succeed(caseClass.rawConstruct(list)) }
        )
      }

      override def render: Mod[HtmlElement] =
        caseClass.parameters.map { param =>
          param.typeclass.label(param.label.capitalize).render
        }
    }

  implicit def gen[A]: Form[A] = macro Magnolia.gen[A]
}
