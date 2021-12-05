package examples

import com.raquo.laminar.api.L._
import examples.Utils.debugForm
import formula.Annotations._
import formula.DeriveForm.gen
import formula.{DeriveForm, FormValue}

import java.time.LocalDate

case class Person(
  name: String
)

object Example {

  // Derived Form
  val derivedPersonForm: FormValue[Person] = DeriveForm.build[Person]

  def example: Div =
    div(
      cls("container"),
      cls("col-md-6"),
      div(h1("🧪 Compile-Time Form Derivation"), margin("20px 0")),
      debugForm("Derived Person Form", derivedPersonForm),
      defaultPersonButton
    )

  private def defaultPersonButton =
    div(
//      button(
//        cls("btn btn-primary btn-sm"),
//        marginTop("20px"),
//        "Set to Default Person",
//        onClick --> { _ =>
//          derivedPersonForm.set(
//            Person(
//              name = "Kit",
//              age = 31,
//            )
//          )
//        }
//      )
    )
}
