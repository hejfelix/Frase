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
  case class State(program: String = "", results: List[String] = Nil)

  private val handleInput = (e: Event) => {
    val program: String = e.target.asInstanceOf[HTMLInputElement].value
    this.setState(s => {
      val steps: List[Either[FraseError, AST.Term]] = interpreter.interpretScan(program)
      println(steps)
      s.copy(results = steps.map(_.fold(_.msg, _.pretty)))
    })
  }

  override def initialState = State()
  override def render(): ReactElement = div(
    input(onChange := handleInput),
    state.results.map(p(_))
  )
}
