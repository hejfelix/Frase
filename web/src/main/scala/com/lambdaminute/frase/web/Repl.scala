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
  case class State(program: String = "", results: List[String] = Nil, stepsToEvaluate: Int = 10)

  private def evaluated: List[String] =
    if (state.program.isEmpty) {
      Nil
    } else {
      val steps: Stream[Either[FraseError, AST.Term]] = interpreter.interpretScan(state.program)
      println(steps)
      steps.take(state.stepsToEvaluate).map(_.fold(_.msg, _.pretty)).toList
    }

  private val handleInput = (e: Event) => {
    val program: String = e.target.asInstanceOf[HTMLInputElement].value
    this.setState(_.copy(program = program))
  }

  private val handleStepsInput = (e: Event) => {
    val number = e.target.asInstanceOf[HTMLInputElement].valueAsNumber
    println(number)
    this.setState { _.copy(stepsToEvaluate = number) }
  }

  override def initialState = State()
  override def render(): ReactElement = div(
    p("Number of steps to evaluate:"),
    input(`type` := "number",
          value := state.stepsToEvaluate.toString,
          onChange := handleStepsInput),
    p("Enter some lambda calculus in the text field to see the beta-reduction"),
    input(onChange := handleInput, size := "80"),
    evaluated.map(step => p(key := step.toString)(step))
  )
}
