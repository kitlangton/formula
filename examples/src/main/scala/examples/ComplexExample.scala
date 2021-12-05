package examples

import com.raquo.laminar.api.L._
import examples.Utils.debugForm
import formula.Annotations._
import formula.DeriveForm.gen
import formula.{DeriveForm, FormValue}

object ComplexExample {

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
  val derivedPersonForm: FormValue[Person] =
    DeriveForm.build[Person]

  def example: Div =
    div(
      marginTop("40px"),
      cls("container"),
      cls("col-md-6"),
      div(
        h1("🧪 Compile-Time Form Derivation")
      ),
      div(height("48px")),
      debugForm("Derived Person Form", derivedPersonForm),
      button(
        cls("btn btn-primary btn-sm"),
        marginTop("20px"),
        "Set to Default Person",
        onClick --> { _ =>
          derivedPersonForm.set(
            Person(
              name = "Kit",
              age = 123,
              isAlive = true,
              rating = 55.5,
              pet = Pet("Crumb"),
              job = Job.HighFiveMachine(33)
            )
          )
        }
      )
    )

}
