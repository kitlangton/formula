package examples

import com.raquo.laminar.api.L
import com.raquo.laminar.api.L._
import examples.Example.{Person, Pet}
import examples.ManualForm.manualPersonForm
import formula.{DeriveForm, FieldLabel, FieldValidation, Form, Validation}

object Example {
  case class Pet(
    @FieldLabel("Pet Name")
    petName: String
  )

  case class Person(
    @FieldValidation[String](_.nonEmpty, "Must not be empty.")
    name: String,
    @FieldValidation[Int](_ > 10, "Must be older than 10")
    @FieldValidation[Int](_ < 90, "Must be younger than 90")
    age: Int,
    rating: Double,
    @FieldLabel("")
    pet: Pet
  )

  // Derived Form
  val derivedPersonForm: Form[Person] =
    DeriveForm.gen

  def example: Div =
    div(
      maxWidth("600px"),
      margin("0 auto"),
      div(
        h1("🧪 Formula"),
        a(color("green"), fontWeight.bold, textDecoration.none, href("https://github.com/kitlangton/formula"), "GitHub")
      ),
      hr(margin("48px 0")),
      debugForm("Derived Person Form", derivedPersonForm),
      br(),
      button(
        "Set to Default Person",
        onClick --> { _ =>
          derivedPersonForm.set(Person("Kit", 123, 55.5, Pet("Crumb")))
        }
      ),
      pre(
        fontSize("14px"),
        padding("8px"),
        background("#ccc"),
        """
case class Pet(
  @FieldLabel("Pet Name")
  petName: String
)

case class Person(
  @FieldValidation[String](_.nonEmpty, "Must not be empty.")
  name: String,
  @FieldValidation[Int](_ > 10, "Must be older than 10")
  @FieldValidation[Int](_ < 90, "Must be younger than 90")
  age: Int,
  rating: Double,
  @FieldLabel("")
  pet: Pet
)

val derivedPersonForm: Form[Person] = DeriveForm.gen
 
def body: HtmlElement = derivedPersonForm.render
        """.trim
      ),
      hr(margin("48px 0")),
      debugForm("Manual Person Form", manualPersonForm),
      br(),
      button(
        "Set to Default Person",
        onClick --> { _ =>
          manualPersonForm.set(Person("Adam", 888, 99.9, Pet("Falcor")))
        }
      )
    )

  private def debugForm[A](name: String, form: Form[A]): Div =
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
          case Validation.Succeed(value)            =>
            value.toString
        }
      )
    )

}

object ManualForm {

  def nameField: Form[String] = Form.string
    .label("Name")
    .validate(_.nonEmpty, "Name must not be empty")
    .validate(_.length < 5, "Name must be short")

  def ageField: Form[Int] = Form.int
    .label("Age")
    .validate((_: Int) > 10, "Age must be greater than 10")

  def ratingField: Form[Double]             =
    Form.dollars
      .label("Money")
      .validate(_.toInt % 2 == 1, "Money must be odd")

  def zipped: Form[((String, Int), Double)] =
    nameField zip ageField zip ratingField

  def makePersonForm: Form[Person] =
    zipped.xmap { case ((name, age), rating) =>
      Person(name, age, rating, Pet("Falcor"))
    } { p =>
      ((p.name, p.age), p.rating)
    }

  val manualPersonForm: Form[Person] = makePersonForm

}
