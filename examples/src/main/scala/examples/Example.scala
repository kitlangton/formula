package examples

import com.raquo.laminar.api.L
import com.raquo.laminar.api.L._
import formula.ZForm.Form
import formula.{DeriveForm, FieldLabel, FieldValidation, Validation, ZForm}

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
      Person(name, age, rating, Pet("Crumb"))
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
          derivedPersonForm.set(Person("Kit", 123, 55.5, Pet("Crumb")))
        }
      ),
      debugForm("Manual Person Form", manualPersonForm),
      br(),
      button(
        "Default Person",
        onClick --> { _ =>
          manualPersonForm.set(Person("Adam", 888, 99.9, Pet("Falcor")))
        }
      )
    )
}
