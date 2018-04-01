package com.lambdaminute.frase.web

import com.lambdaminute.frase.calculus.ast.AST
import com.lambdaminute.frase.calculus.errors.FraseError
import com.lambdaminute.frase.calculus.grammar.{DefaultLexer, DefaultParser}
import com.lambdaminute.frase.calculus.interpreter.{DefaultBuiltins, DefaultInterpreter}
import com.lambdaminute.frase.calculus.semantic.DefaultKeywords
import org.scalajs.dom.Event
import org.scalajs.dom.raw.HTMLInputElement
import slinky.core.Component
import slinky.core.annotations.react
import slinky.core.facade.ReactElement
import slinky.web.html._

@react class Repl extends Component {

  private val defaultKeywords = DefaultKeywords()
  private val lexer           = DefaultLexer()
  private val parser          = DefaultParser(lexer)
  private val defaultBuiltins = DefaultBuiltins.builtIns
  private val interpreter =
    DefaultInterpreter(parser, defaultKeywords, defaultBuiltins)

  case class Props()
  case class State(program: String = "", stepsToEvaluate: Int = 30, evaluated: List[String] = Nil)

  private def evaluate(program: String): List[String] =
    if (program.isEmpty) {
      Nil
    } else {
      val steps: Stream[Either[FraseError, AST.Term]] = interpreter.interpretScan(program)
      steps.map(_.fold(_.msg, _.pretty)).take(state.stepsToEvaluate).toList
    }

  private val handleInput = (e: Event) => {
    val program: String = e.target.asInstanceOf[HTMLInputElement].value
    this.setState(_.copy(program = program, evaluated = evaluate(program)))
  }

  private val handleStepsInput = (e: Event) => {
    val number = e.target.asInstanceOf[HTMLInputElement].valueAsNumber
    this.setState(s => s.copy(stepsToEvaluate = number, evaluated = evaluate(s.program)))
  }

  override def initialState = State()
  override def render(): ReactElement = div(
    p("Number of steps to evaluate:"),
    input(`type` := "number", value := state.stepsToEvaluate.toString, onChange := handleStepsInput),
    p("Enter some lambda calculus in the text field to see the beta-reduction"),
    input(onChange := handleInput, size := "80"),
    br(),
    br(),
    s"Last evaluated result: ",
    b(state.evaluated.takeRight(1).mkString),
    s" (total steps: ${state.evaluated.length})",
    br(),
    br(),
    state.evaluated.map(step => p(key := step.toString)(step))
  )
}
