package examples

import com.raquo.laminar.api.L
import com.raquo.laminar.api.L._
import com.raquo.laminar.nodes.ReactiveHtmlElement
import formula.ZForm.Form
import formula._
import org.scalajs.dom

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.util.Try

@js.native
@JSImport("stylesheets/main.scss", JSImport.Namespace)
object Css extends js.Any

object Main {
  val css: Css.type = Css

  def main(args: Array[String]): Unit = {
    val _ = documentEvents.onDomContentLoaded.foreach { _ =>
      val appContainer = dom.document.querySelector("#app")
      appContainer.innerHTML = ""
      val _ = render(appContainer, Example.example)
    }(unsafeWindowOwner)
  }

}

object Example {
  case class Person(name: String, age: Int, rating: Double)

  def nameField: Form[String] = ZForm.string
    .label("Name")
    .validate(_.nonEmpty, "Name must not be empty")
    .validate(_.length < 5, "Name must be short")

  val ageField: Form[Int] = ZForm.int
    .label("Age")
    .validate((_: Int) > 10, "Age must be greater than 10")

  val ratingField: Form[Double] = ZForm.double
    .label("Rating")
    .validate(_ > 0, "Rating must be greater than 1")

  def zipped: Form[((String, Int), Double)] =
    nameField zip ageField zip ratingField

  def makePersonForm: Form[Person] =
    zipped.xmap { case ((name, age), rating) =>
      Person(name, age, rating)
    } { p =>
      ((p.name, p.age), p.rating)
    }

  val manualPersonForm: Form[Person] = makePersonForm

  val derivedPersonForm: Form[Person] = DeriveForm.gen

  def debugForm[A](name: String, form: Form[A]): Div =
    div(
      h2(name),
      L.form(
        form.render
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
          case Validation.Succeed(value) =>
            value.toString
        }
      )
    )

  def example: Div =
    div(
      debugForm("Derived Person Form", derivedPersonForm),
      br(),
      button(
        "Default Person",
        onClick --> { _ =>
          derivedPersonForm.set(Person("Kit", 123, 55.5))
        }
      ),
      debugForm("Manual Person Form", manualPersonForm),
      br(),
      button(
        "Default Person",
        onClick --> { _ =>
          manualPersonForm.set(Person("Adam", 888, 99.9))
        }
      )
    )
}
