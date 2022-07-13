package examples

import java.time.LocalDate

case class Person(
  name: String = "Kit Langton",
  age: Int,
  birthday: LocalDate,
  pet: Pet
)

final case class Pet(
  petName: String,
  petType: PetType
)

sealed trait PetType extends Product with Serializable

object PetType {
  case object Parrot extends PetType
  case object Lizard extends PetType
  case object Dog    extends PetType
  case object Cat    extends PetType
}
