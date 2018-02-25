package com.lambdaminute.frase.web

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
  case class State(program: String = "", result: String = "")

  private val handleInput = (e: Event) => {
    val inputelement: String = e.target.asInstanceOf[HTMLInputElement].value
    println(inputelement)
    this.setState(_.copy(program = inputelement))
  }

  private val handleClick = (e: Event) => this.setState(s => s.copy(result = interpreter.interpret(s.program).toString))

  override def initialState = State()
  override def render(): ReactElement = div(
    input(onChange := handleInput),
    button(onClick := handleClick)("evaluate"),
    p(state.result.mkString)
  )
}
