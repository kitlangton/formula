package examples

import com.raquo.laminar.api.L._
import examples.Utils.makeForm
import formula.Annotations.validation
import formula.DeriveForm.gen

object Example {
  def example: Div =
    div(
      h1(margin("20px 0"), "Compile-Time Derivation 🧪"),
      makeForm[Person]("Derived Person Form")
    )
}
