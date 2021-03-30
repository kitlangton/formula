package formula

import com.raquo.laminar.api.L._
import magnolia._

import scala.language.experimental.macros

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

  def ~[B](that: Form[B]): Form[(A, B)] = new Form[(A, B)] {
    override def renderImpl(variable: Var[(A, B)])(implicit owner: Owner): Mod[HtmlElement] =
      Seq(
        self.renderImpl(variable.zoom(_._1)(_ -> variable.now()._2)),
        that.renderImpl(variable.zoom(_._2)(variable.now()._1 -> _))
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

  implicit val int: Form[Int] = string.xmap(_.toInt)(_.toString)

  def render[A](variable: Var[A])(implicit form: Form[A]): FormElement = form.render(variable)

//  val exampleManual: Form[Person] =
//    (string.labelled("Name") ~ string.labelled("Email") ~ int.labelled("Age"))
//      .xmap[Person] { case ((name, email), age) =>
//        Person(name, email, age)
//      } { case Person(name, email, age) =>
//        ((name, email), age)
//      }
}

object Formula {
  def example: HtmlElement =
    div(
      h4("Formula"),
      Form.render(personVar),
      child.text <-- personVar.signal.map(_.toString),
      button(
        "GET OLDER",
        onClick --> { _ =>
          personVar.update(person => person.copy(age = person.age + 5))
        }
      )
    )

  case class Person(
      name: String,
      email: String,
      favoriteFood: String,
      age: Int,
      dog: Dog
  )

  object Person {
    implicit val personForm: Form[Person] = DeriveForm.gen[Person]
  }

  case class Dog(nickname: String, loudness: Int)

  lazy val personVar = Var(
    Person("Kit", "kit.langton@fakemail.com", "Ground Beef", 30, Dog("Crunchy", 10))
  )

  def mainForm: HtmlElement =
    form(
      formInput(
        "Name",
        "text",
        personVar.signal.map(_.name),
        { string =>
          personVar.update(_.copy(name = string))
        }
      ),
      formInput(
        "Email",
        "text",
        personVar.signal.map(_.email),
        { string =>
          personVar.update(_.copy(email = string))
        }
      ),
      formInput(
        "Age",
        "number",
        personVar.signal.map(_.age.toString),
        { string =>
          personVar.update(_.copy(age = string.toInt))
        }
      )
    )

  private def formInput(
      name: String,
      tpe: String = "text",
      signal: Signal[String],
      update: String => Unit
  ): HtmlElement =
    div(
      cls("input-group"),
      label(name),
      input(
        `type`(tpe),
        placeholder("Name"),
        controlled(
          value <-- signal,
          onInput.mapToValue --> update
        )
      )
    )
}
