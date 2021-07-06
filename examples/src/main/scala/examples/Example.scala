package examples

import com.raquo.laminar.api.L._
import examples.ManualForm.{ManualPerson, manualPersonForm}
import examples.Utils.debugForm
import formula.DeriveForm.gen
import formula.Form.{FormValidation, FormValue}
import formula.{DeriveForm, FieldLabel, FieldValidation, Form}

object Example {

  sealed trait Job

  object Job {
    case object Clown                                     extends Job
    case class Doctor(wearingStethoscope: Boolean = true) extends Job
    case class HighFiveMachine(highFivesPerSecond: Int)   extends Job
  }

  case class Pet(petName: String)

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
  val derivedPersonForm: Form.FormValue[Person] = DeriveForm.build[Person]

  sealed trait Media
  sealed trait Proposal

  case class Author(name: String)

  object Author {
    implicit val form: Form[Author] =
      Form.select(
        List(Author("J.B. Bobo"), Author("Kathy Kathy Kathy"), Author("Xyzzxyx Cthulu"), Author("Herman Melville"))
      )(_.name)
  }

  object Proposal {
    case class FilmProposal(title: String, director: String, year: Int)                     extends Proposal
    case class LiteratureProposal(title: String, read: Boolean, author: Author, pages: Int) extends Proposal
    case class MusicProposal(name: String, artist: String, duration: Int)                   extends Proposal
  }

  val proposalForm = DeriveForm.build[Proposal]

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
          derivedPersonForm.set(Person("Kit", 123, true, 55.5, Pet("Crumb"), Job.HighFiveMachine(33)))
        }
      ),
      div(em("The Code"), marginTop("12px")),
      pre(
        fontSize("14px"),
        padding("8px"),
        background("#ccc"),
        """
  case class Pet(
    @FieldLabel("Pet Name")
    petName: String
  )

  sealed trait Job

  object Job {
    case object Clown           extends Job
    case object Doctor          extends Job
    @FieldLabel("High Five Machine")
    case class HighFiveMachine(highFivesPerSecond: Int) extends Job
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
      ),
      hr(margin("48px 0")),
      debugForm("Media Form", proposalForm),
      button(
        "MOBY DICK",
        onClick --> { _ =>
          proposalForm.set(Proposal.LiteratureProposal("Moby Dick", true, Author("Herman Melville"), 39))
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
