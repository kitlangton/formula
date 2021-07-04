package examples

import com.raquo.laminar.api.L
import com.raquo.laminar.api.L._
import formula.Form.FormValue
import formula.Validation

object Utils {
  def debugForm[A](name: String, form: FormValue[A]): Div =
    div(
      h2(name),
      L.form(
        form.node
      ),
      pre(
        background("#ddd"),
        child <-- form.signal.map {
          case Validation.Warnings(warnings, value) =>
            pre(
              value.toString,
              pre(
                color("red"),
                warnings.mkString("\n")
              )
            )
          case Validation.Succeed(value)            =>
            value.toString
        }
      )
    )
}
