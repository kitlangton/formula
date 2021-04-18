package formula

import com.raquo.laminar.api.L._
import magnolia._

import scala.language.experimental.macros
import java.util.UUID

object DeriveForm {
  type Typeclass[A] = Form[A]

  def combine[A](caseClass: CaseClass[Form, A]): Form[A] = new Form[A] {
    private def zoomToParam(variable: Var[A], param: Param[Typeclass, A])(implicit owner: Owner): Var[param.PType] =
      variable.zoom[param.PType](a => param.dereference(a))(value =>
        caseClass.construct { p =>
          if (p == param) value
          else p.dereference(variable.now())
        }
      )

    override def renderImpl(variable: Var[A])(implicit owner: Owner): Mod[HtmlElement] =
      caseClass.parameters.map { param =>
        val paramVar = zoomToParam(variable, param)
        param.typeclass.labelled(param.label).renderImpl(paramVar)
      }.toList
  }

  implicit def gen[A]: Form[A] = macro Magnolia.gen[A]
}

sealed trait Form[A] { self =>
  def labelled(str: String): Form[A] = new Form[A] {
    override def renderImpl(variable: Var[A])(implicit owner: Owner): Mod[HtmlElement] =
      div(
        cls("input-group"),
        label(str),
        self.renderImpl(variable)
      )
  }

  def xmap[B](to: A => B)(from: B => A): Form[B] = new Form[B] {
    override def renderImpl(variable: Var[B])(implicit owner: Owner): Mod[HtmlElement] =
      self.renderImpl(variable.zoom[A](from)(to))
  }

  private[formula] def renderImpl(variable: Var[A])(implicit owner: Owner): Mod[HtmlElement]

  def render(variable: Var[A]): FormElement =
    form(
      onMountInsert { ctx =>
        div(renderImpl(variable)(ctx.owner))
      }
    )
}

object Form {
  implicit val string: Form[String] = new Form[String] {
    override def renderImpl(variable: Var[String])(implicit owner: Owner): HtmlElement =
      input(
        controlled(
          value <-- variable,
          onInput.mapToValue --> variable
        )
      )    
  }

  implicit val bool: Form[Boolean] = new Form[Boolean] {
    override def renderImpl(variable: Var[Boolean])(implicit owner: Owner): HtmlElement =
      input(
        //controlled(
          typ("checkbox"),          
          onInput.mapToChecked --> variable
        )
      //)    
  }

    implicit val uuid: Form[UUID] = new Form[UUID] {
    override def renderImpl(variable: Var[UUID])(implicit owner: Owner): HtmlElement =
      input(
        controlled(
          value <-- variable.zoom(_.toString())(UUID.fromString(_)),
          onInput.mapToValue --> variable.zoom(_.toString())(UUID.fromString(_))
        ),
        readOnly := true,// I often want to be able to see an ID. It is rare, that I wish to be able to edit one.
      )      
  }

    implicit val int: Form[Int] = new Form[Int] {
    override def renderImpl(variable: Var[Int])(implicit owner: Owner): HtmlElement =
      input(
        controlled(
          value <-- variable.zoom(_.toString())(_.toInt),
          // Constrain the input strings to accept numbers only... prevents nasty exceptions.
          onInput.mapToValue.filter(_.forall(Character.isDigit)) --> variable.zoom[String](_.toString())(_.toInt)
        )
      )    
  }

  //The double implementation below is kind of nasty. But I think it works. Could be improved with a better error message perhaps.
  implicit val dble: Form[Double] = new Form[Double] {
    override def renderImpl(variable: Var[Double])(implicit owner: Owner): HtmlElement = {
      def testFct(s:String) : Boolean = s.toDoubleOption.isDefined

      val intermediate = variable.zoom[String](_.toString())(_.toDouble)
      val fake = Var(intermediate.now())
      
      input(
          value <-- intermediate,
          onInput.mapToValue.filter({ina => 
            fake.set(ina);
            testFct(ina)
          }) --> intermediate,
          borderColor <-- fake.zoom[String](in => if(testFct(in)) "" else "red" )(x => "")
        )    
    }
  }

    
    implicit val bigD: Form[BigDecimal] = new Form[BigDecimal] {
    override def renderImpl(variable: Var[BigDecimal])(implicit owner: Owner): HtmlElement =
      input(      
          value <-- variable.zoom(_.toString())(BigDecimal(_)),
          
          onInput.mapToValue -->  variable.zoom[String](_.toString())(BigDecimal(_))        
      )
    }    
  

  def render[A](variable: Var[A])(implicit form: Form[A]): FormElement = form.render(variable)
}

object Formula {
  def example: HtmlElement =
    div(
      h4("Formula"),
      Form.render(personVar),
      child.text <-- personVar.signal.map(_.toString),
      button(
        "Boost aweseome",
        onClick --> { _ =>
          personVar.update(person => person.copy(theAwesome = person.theAwesome + 5))
        }
      )
    )

  case class Person(
      uuid: UUID,
      name: String,
      workyWorky: Boolean,
      age: Int,
      theAwesome: Double,
      bigD: BigDecimal,
      dog: Dog
  )

  object Person {
    implicit val personForm: Form[Person] = DeriveForm.gen[Person]
  }

  case class Dog(nickname: String, loudness: Int)

  lazy val personVar = Var(
    Person(java.util.UUID.randomUUID, "Kit", false, 30, 30.001 , 1.0, Dog("Not crunchy", 10))
  )
}
