package com.lambdaminute.frase.web

import com.lambdaminute.frase.calculus.ast.Ast
import com.lambdaminute.frase.calculus.errors.FraseError
import com.lambdaminute.frase.calculus.grammar.{DefaultLexer, DefaultParser}
import com.lambdaminute.frase.calculus.interpreter.{DefaultBuiltins, DefaultInterpreter}
import com.lambdaminute.frase.calculus.semantic.DefaultKeywords
import org.scalajs.dom
import org.scalajs.dom.Event
import org.scalajs.dom.raw.{Element, HTMLInputElement, NodeList}
import slinky.core.{Component, CustomAttribute, CustomTag}
import slinky.core.annotations.react
import slinky.core.facade.ReactElement
import slinky.web.html._

import scala.scalajs.js
import scala.scalajs.js.Dynamic

@react class Repl extends Component {

  private val expressionLengthCutoff = 90
  private val defaultKeywords        = DefaultKeywords()
  private val lexer                  = DefaultLexer()
  private val parser                 = DefaultParser(lexer)
  private val defaultBuiltins        = DefaultBuiltins.builtIns

  private val interpreter =
    DefaultInterpreter(parser, defaultKeywords, defaultBuiltins)
  type Props = Unit

  case class State(program: String = "", stepsToEvaluate: Int = 30, evaluated: List[String] = Nil)
  private def evaluate(program: String): List[String] =
    if (program.isEmpty) {
      Nil
    } else {
      val steps: Stream[Either[FraseError, Ast.Term]] = interpreter.interpretScan(program)
      steps.map(_.fold(_.msg, _.pretty)).take(state.stepsToEvaluate).toList
    }

  private val handleInput = (e: Event) => {
    val program: String = e.target.asInstanceOf[HTMLInputElement].value
    this.setState(_.copy(program = program, evaluated = evaluate(program)))
    println(dom.document.body.scrollHeight)
    dom.window.scrollTo(0, dom.document.body.scrollHeight)
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

  def codeInput =
    div(className := "mdc-text-field", style := Dynamic.literal("marginTop" -> "1em"))(
      input(
        `type` := "text",
        onChange := handleInput,
        size := "80",
        id := "code",
        className := "mdc-text-field__input",
      ),
      label(className := "mdc-floating-label", htmlFor := "code")("Enter lambda calculus to be evaluated"),
      div(className := "mdc-line-ripple")
    )

  override def render(): ReactElement = div(
    lineInput,
    codeInput,
    br(),
    br(),
    s"Last evaluated result: ",
    b(state.evaluated.takeRight(1).mkString),
    s" (total steps: ${state.evaluated.length})",
    br(),
    br(),
    state.evaluated.zipWithIndex.map {
      case (step, idx) =>
        div(className := "mdc-card", key := step, style := Dynamic.literal("marginTop" -> "1em", "padding" -> "1em"))(
          i(style := Dynamic.literal("color" -> "#aaa"))(s"Step $idx >"),
          p(s"${step.toString}"))
    }
  )
}
