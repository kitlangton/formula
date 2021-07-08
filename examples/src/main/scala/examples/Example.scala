package examples

import com.raquo.laminar.api.L._
import examples.ManualForm.{ManualPerson, manualPersonForm}
import examples.Utils.debugForm
import formula.DeriveForm.gen
import formula.{DeriveForm, Form, FormValue}
import formula.Annotations._

object Example {

  sealed trait Job

  object Job {
    case object Clown                                   extends Job
    case class Doctor(
      @validation[Boolean](identity, "Must be wearing stethoscope")
      wearingStethoscope: Boolean = true
    )                                                   extends Job
    case class HighFiveMachine(highFivesPerSecond: Int) extends Job
  }

  case class Pet(petName: String)

  case class Person(
    @help("People generally have names.")
    @validation[String](_.nonEmpty, "Must not be empty.")
    name: String,
    @validation[Int](_ > 10, "Must be older than 10")
    @validation[Int](_ < 90, "Must be younger than 90")
    age: Int,
    @help("Denotes whether or not this person is currently 'among the living'.")
    isAlive: Boolean,
    rating: Double,
    @label("")
    pet: Pet,
    job: Job
  )

  // Derived Form
  val derivedPersonForm: FormValue[Person] = DeriveForm.build[Person]

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

  val ones = Form.select(List(1, 2, 3))(_.toString)
  val twos = Form.select(List(4, 5, 6))(_.toString)

  val proposalForm = DeriveForm
    .gen[Proposal]
    .flatZip {
      case Proposal.FilmProposal(title, director, year)            => ones
      case Proposal.LiteratureProposal(title, read, author, pages) => twos
      case Proposal.MusicProposal(name, artist, duration)          =>
        Form.select(List(7, 8, duration))(_.toString)
    }
    .build

//  val $numbers = proposalForm.$value.map {
//    case Proposal.FilmProposal(title, director, year)            => List(1, 2, 3)
//    case Proposal.LiteratureProposal(title, read, author, pages) => List(4, 5, 6)
//    case Proposal.MusicProposal(name, artist, duration)          => List(7, 8, 9)
//  }

//  val numberForm = Form.render(Form.select($numbers)(_.toString))

  def example: Div =
    div(
      cls("container"),
      cls("col-md-6"),
      div(
        h1("🧪 Formula"),
        a(color("green"), fontWeight.bold, textDecoration.none, href("https://github.com/kitlangton/formula"), "GitHub")
      ),
      div(height("48px")),
      debugForm("Derived Person Form", derivedPersonForm),
      button(
        cls("btn btn-primary btn-sm"),
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
sealed trait Job

object Job {
  case object Clown                                   extends Job
  case class Doctor(
    @validation[Boolean](identity, "Must be wearing stethoscope")
    wearingStethoscope: Boolean = true
  )                                                   extends Job
  case class HighFiveMachine(highFivesPerSecond: Int) extends Job
}

case class Pet(petName: String)

case class Person(
  @validation[String](_.nonEmpty, "Must not be empty.")
  @help("People generally have names.")
  name: String,
  @validation[Int](_ > 10, "Must be older than 10")
  @validation[Int](_ < 90, "Must be younger than 90")
  age: Int,
  @help("Denotes whether or not this person is currently 'among the living'.")
  isAlive: Boolean,
  rating: Double,
  @label("")
  pet: Pet,
  job: Job
)
 
def body: HtmlElement = DeriveForm.build[Person].node
        """.trim
      ),
      hr(margin("48px 0")),
      debugForm("Manual Person Form", manualPersonForm),
      br(),
      button(
        cls("btn btn-primary btn-sm"),
        "Set to Default Person",
        onClick --> { _ =>
          manualPersonForm.set(ManualPerson("J.B. Bobo", 55, true, 99.99))
        }
      ),
      hr(margin("48px 0")),
      debugForm("Media Form", proposalForm),
//      debugForm("NUMBERS", numberForm),
      button(
        cls("btn btn-primary btn-sm"),
        "MOBY DICK",
        onClick --> { _ =>
          proposalForm.set(Proposal.LiteratureProposal("Moby Dick", true, Author("Herman Melville"), 39) -> 10)
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

  val manualPersonForm: FormValue[ManualPerson] = Form.build(makePersonForm)

}
