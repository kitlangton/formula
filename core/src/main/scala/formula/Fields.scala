package formula

import com.raquo.laminar.api.L._
import formula.Form.{FormVar, InputConfig}

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import scala.util.Try
import scala.util.matching.Regex

private[formula] object Fields {

  // Regex Constrained Field
  def regex[A](config: InputConfig[A], variable: FormVar[A], parser: String => Option[A], regex: Regex): HtmlElement = {
    val stringVar = Var("")

    val touched = Var(false)

    input(
      config.modifiers,
      stringVar.signal.changes.map(parser).collect { case Some(d) => d } --> { value => variable.set(value) },
      variable.signal.changes.map(_.value.toString) --> stringVar,
      onKeyPress --> { event =>
        if (event.key.length == 1 && !regex.matches(event.key) && !event.metaKey) event.preventDefault()
        touched.set(true)
      },
      cls.toggle("is-invalid") <-- variable.signal.combineWithFn(touched.signal) {
        _.warnings.nonEmpty && _
      },
      inContext { el =>
        controlled(
          value <-- stringVar,
          onInput.mapToValue --> { value =>
            if (regex.matches(value)) {
              stringVar.set(value)
            } else {
              val start = Try(el.ref.selectionStart).getOrElse(0)
              el.ref.value = stringVar.now()
              el.ref.selectionStart = start - 1
              el.ref.selectionEnd = start - 1
            }
          }
        )
      }
    )
  }

  def date(config: InputConfig[LocalDate], variable: FormVar[LocalDate]): HtmlElement = {
    val formatter = DateTimeFormatter.ISO_DATE

    input(
      config.modifiers,
      `type`("date"),
      controlled(
        value <-- variable.signal.map(_.value.format(formatter)),
        onInput.mapToValue --> { text =>
          val result = LocalDate.parse(text, formatter)
          variable.set(result)
        }
      )
    )
  }

  // Money Field
  private val moneyRegex: Regex = "\\$? ?\\d{1,3}[,\\d{1,3}]*\\.?\\d{0,2}".r
  def makeDollars(inputConfig: InputConfig[Double]): FormValue[Double] = {
    val var0: FormVar[Double] =
      FormVar.make(0.0)

    def renderMoney(string: String): String =
      "$ " + string.filter(c => c.isDigit || c == '.').takeWhile(_ != '.').reverse.grouped(3).mkString(",").reverse +
        string.dropWhile(_ != '.')

    val node: Mod[HtmlElement] = {
      val stringVar     = Var("")
      val lastSelection = Var(0 -> 0)
      val lastKeyDown   = Var("")

      def parseDouble(string: String): Option[Double] =
        Option
          .when(moneyRegex.matches(string)) {
            string.filter(c => c.isDigit || c == '.').toDoubleOption
          }
          .flatten

      div(
        stringVar.signal.changes.map(parseDouble).collect { case Some(value) => value } --> { str =>
          var0.set(str)
        },
        input(
          inputConfig.modifiers,
          var0.signal.map(s => renderMoney(s.value.toString)) --> stringVar,
          value <-- stringVar.signal,
          inContext { el =>
            onInput.mapToValue --> { string =>
              parseDouble(string) match {
                case Some(_) =>
                  val rendered = renderMoney(string)
                  stringVar.set(rendered)
                  el.ref.value = stringVar.now()

                  val (start, end) = lastSelection.now()
                  val diff         = rendered.drop(start).takeWhile(_ != lastKeyDown.now().head).length + 1
                  el.ref.selectionStart = start + diff
                  el.ref.selectionEnd = end + diff
                case None    =>
                  el.ref.value = stringVar.now()

                  val (start, end) = lastSelection.now()
                  el.ref.selectionStart = start
                  el.ref.selectionEnd = end
              }
            }
          },
          inContext { el =>
            onKeyDown --> { ev =>
              lastKeyDown.set(ev.key)
              lastSelection.set(el.ref.selectionStart -> el.ref.selectionEnd)
            }
          }
        )
      )
    }
    FormValue(var0, node)
  }
}
