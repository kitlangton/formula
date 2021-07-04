package examples

import com.raquo.laminar.api.L._
import examples.ManualForm.{ManualPerson, manualPersonForm}
import examples.Utils.debugForm
import formula.Form.FormValue
import formula.{DeriveForm, FieldLabel, FieldValidation, Form}

object Example {
  case class Pet(
    @FieldLabel("Pet Name")
    petName: String
  )

  sealed trait Job

  object Job {
    case object Clown           extends Job
    case object Doctor          extends Job
    @FieldLabel("High Five Machine")
    case object HighFiveMachine extends Job
  }

  case class Person(
    @FieldValidation[String](_.nonEmpty, "Must not be empty.")
    name: String,
    @FieldValidation[Int](_ > 10, "Must be older than 10")
    @FieldValidation[Int](_ < 90, "Must be younger than 90")
    age: Int,
    isAlive: Boolean,
    rating: Double,
    @FieldLabel("")
    pet: Pet,
    job: Job
  )

  // Derived Form
  val derivedPersonForm: Form.FormValue[Person] =
    Form.render(DeriveForm.gen[Person])

//  val weirdForm: Form[(Boolean, String)] = Form.boolean.flatZip { b =>
//    if (b) Form.string.label("True")
//    else Form.string.label("False")
//  }
//
//  val weird = Form.render(Form.int.label("COOL").many)

  def example: Div =
    div(
      maxWidth("600px"),
      margin("0 auto"),
      div(
        h1("🧪 Formula"),
        a(color("green"), fontWeight.bold, textDecoration.none, href("https://github.com/kitlangton/formula"), "GitHub")
      ),
      div(height("48px")),
      debugForm("Derived Person Form", derivedPersonForm),
      button(
        "Set to Default Person",
        onClick --> { _ =>
          derivedPersonForm.set(Person("Kit", 123, true, 55.5, Pet("Crumb"), Job.Clown))
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
          manualPersonForm.set(ManualPerson("J.B. Bobo", 55, true, 99.99))
        }
      )
    )

}

object ManualForm {
  case class ManualPerson(
    name: String,
    age: Int,
    isAlive: Boolean,
    rating: Double
  )

  val nameField: Form[String] = Form.string
    .label("Name")
    .validate(_.nonEmpty, "Name must not be empty")
    .validate(_.length < 5, "Name must be short")

  val ageField: Form[Int] = Form.int
    .label("Age")
    .validate((_: Int) > 10, "Age must be greater than 10")

  val isAliveField: Form[Boolean] = Form.boolean
    .label("Is Alive")

  val ratingField: Form[Double]                        =
    Form.dollars
      .label("Money")
      .validate(_.toInt % 2 == 1, "Money must be odd")

  val zipped: Form[(((String, Int), Boolean), Double)] =
    nameField zip ageField zip isAliveField zip ratingField

  val makePersonForm: Form[ManualPerson] =
    zipped.xmap { case (((name, age), isAlive), rating) =>
      ManualPerson(name, age, isAlive, rating)
    } { p =>
      (((p.name, p.age), p.isAlive), p.rating)
    }

  val manualPersonForm: FormValue[ManualPerson] = Form.render(makePersonForm)

}
