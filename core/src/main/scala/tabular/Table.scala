package tabular

import magnolia1._
import tabular.Table.{Field, Fields}

import scala.language.experimental.macros

trait DeriveTable[A] {
  def derive(value: A): Table[A]
}

sealed trait Table[+A] {}

object Table {
  final case class Fields[A](list: List[(String, String)]) extends Table[A]
  final case class Field[A](value: String)                 extends Table[A]
}

object DeriveTable {
  type Typeclass[A] = DeriveTable[A]

  def join[A](caseClass: CaseClass[DeriveTable, A]): DeriveTable[A] =
    new DeriveTable[A] {
      override def derive(value: A): Table[A] = {
        val fields = caseClass.parameters.flatMap { param =>
          param.typeclass.derive(param.dereference(value)) match {
            case Fields(list) => list
            case Field(value) => List(param.label -> value)
          }
        }
        Fields(fields.toList)
      }
    }

  def split[A](sealedTrait: SealedTrait[DeriveTable, A]): DeriveTable[A] =
    new DeriveTable[A] {
      def derive(value: A): Table[A] =
        sealedTrait
          .split(value) { subtype =>
            subtype.typeclass.derive(subtype.cast(value))
          }
//          .asInstanceOf[Table[A]]
    }

  def fallback[A]: DeriveTable[A] = new DeriveTable[A] {
    override def derive(value: A): Table[A] = Field(value.toString)
  }

  implicit def gen[A]: DeriveTable[A] = macro Magnolia.gen[A]
}
