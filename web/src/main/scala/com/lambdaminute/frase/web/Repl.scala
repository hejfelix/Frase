package com.lambdaminute.frase.web

import cats.implicits._
import com.lambdaminute.frase.calculus.ast.Ast.Term
import com.lambdaminute.frase.calculus.errors.FraseError
import com.lambdaminute.frase.calculus.grammar.{DefaultLexer, DefaultParser}
import com.lambdaminute.frase.calculus.interpreter._
import com.lambdaminute.frase.calculus.semantic.DefaultKeywords
import org.scalajs.dom
import org.scalajs.dom.Event
import org.scalajs.dom.raw.{HTMLInputElement, NodeList}
import slinky.core.{Component, StateReaderProvider}
import slinky.core.annotations.react
import slinky.core.facade.ReactElement
import slinky.web.html._

import scala.scalajs.js.Dynamic

@react class Repl extends Component {

  private val defaultKeywords = DefaultKeywords()
  private val lexer           = DefaultLexer()
  private val parser          = DefaultParser(lexer)
  private val defaultBuiltins = DefaultBuiltins.builtIns

  private val interpreter =
    DefaultInterpreter(parser, defaultKeywords, defaultBuiltins)
  type Props = Unit

  case class State(program: String = "",
                   stepsToEvaluate: Int = 30,
                   evaluated: Either[String, List[(List[Trace], Term)]] = Either.right(Nil))

  private def evaluate(program: String): Either[String, List[(List[Trace], Term)]] =
    if (program.isEmpty) {
      Either.right(Nil)
    } else {
      val steps: Either[FraseError, Stream[(List[Trace], Term)]] = interpreter.traceInterpret(program)
      println(steps.map(_.toList.mkString("\n")))
      steps.map(_.take(state.stepsToEvaluate).toList).leftMap(_.msg)
    }

  private val handleInput = (e: Event) => {
    val program: String = e.target.asInstanceOf[HTMLInputElement].value
    this.setState(_.copy(program = program, evaluated = evaluate(program)))
  }

  private val handleStepsInput = (e: Event) => {
    val number = e.target.asInstanceOf[HTMLInputElement].valueAsNumber.toInt
    this.setState(s => s.copy(stepsToEvaluate = number, evaluated = evaluate(s.program)))
  }

  override def initialState = State()

  private def lineInput =
    div(className := "mdc-text-field")(
      input(`type` := "number",
            id := "line-number",
            className := "mdc-text-field__input",
            value := state.stepsToEvaluate.toString,
            onChange := handleStepsInput),
      label(className := "mdc-floating-label", htmlFor := "my-text-field")("Number of steps to evaluate"),
      div(className := "mdc-line-ripple")
    )

  override def componentDidMount() {
    val inputElements: NodeList = dom.document.querySelectorAll(".mdc-text-field")
    val n                       = inputElements.length
    for (i <- 0 until n) {
      val inputElement = inputElements(i)
      println(inputElement)
      Dynamic.newInstance(Dynamic.global.mdc.textField.MDCTextField)(inputElement)
    }
  }

  private val resultStyle = Dynamic.literal(
    "marginTop"       -> "1em",
    "padding"         -> "1em",
    "borderRadius"    -> "5px",
    "backgroundColor" -> "#f0f6ff"
  )

  private val traceStyle = Dynamic.literal(
    "text-align" -> "right",
    "color" -> "#4d6aa2"
  )

  def codeInput =
    div(className := "mdc-text-field", style := Dynamic.literal("marginTop" -> "1em"))(
      input(
        `type` := "text",
        onChange := handleInput,
        size := "80",
        id := "code",
        className := "mdc-text-field__input",
        style := Dynamic.literal("fontFamily" -> "monospace")
      ),
      label(className := "mdc-floating-label", htmlFor := "code")("Enter lambda calculus to be evaluated"),
      div(className := "mdc-line-ripple")
    )

  private def results: ReactElement =
    div(className := "mdc-card", style := Dynamic.literal("marginTop" -> "1em", "padding" -> "1em"))(
      state.evaluated match {
        case Left(err) => p(s"Error: ${err}")
        case Right(trace) =>
          trace.zipWithIndex.map {
            case ((traces, step), idx) =>
              div(style := Dynamic.literal("marginTop" -> "1em"))(
                code(i(style := Dynamic.literal("color" -> "#aaa"))(s"Step $idx > ")),
                br(),
                code(step.pretty),
                br(),
                br(),
                code(style := traceStyle)(showTrace(traces))
              )
          }
      }
    )

  private def showTrace(traces: List[Trace]): ReactElement = div(
    traces
      .flatMap {
        case ApplicationTrace(variable, subst, in) =>
          val res: List[ReactElement] =
            List[ReactElement](code(s"${variable.pretty} ← ${subst.pretty}"),
                               sub(b("  in  ": ReactElement)),
                               code(in.pretty))
          println(res)
          res
        case BuiltInTrace(term, substitution) =>
          List[ReactElement](
            code(s"${term.pretty}  ← ", code(substitution.pretty), sub(b(" (builtin)")))
          )
      }
  )

  override def render(): ReactElement = div(
    lineInput,
    codeInput,
    state.evaluated match {
      case Left(err) => div()
      case Right(evaluated) =>
        div(className := "mdc-elevation--z3", style := resultStyle)(
          b(code(evaluated.takeRight(1).map(_._2.pretty).mkString)),
          br(),
          s" (total steps: ${evaluated.length})",
        )
    },
    results
  )
}
