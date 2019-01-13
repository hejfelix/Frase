package com.lambdaminute.frase.web

import slinky.core._
import slinky.core.annotations.react
import slinky.core.facade.ReactElement
import slinky.web.html.{style, _}

import scala.scalajs.js
import scala.scalajs.js.Dynamic
import scala.scalajs.js.annotation.JSImport

@JSImport("resources/index.css", JSImport.Default)
@js.native
object IndexCss extends js.Object

@react class App extends StatelessComponent {
  type Props = Unit

  private val examples: List[(String, String)] = List(
    "+ 41 1"         -> "Adds 41 and 1 by applying the function `+` first to 41, then to 1 to produce the result",
    "(x . + 40 x) 2" -> "The left parenthesis is a lambda abstraction, allowing x to be replaced upon application (here, with 2)",
    "true yes no"    -> "`true` and `false` are functions in Frase, i.e. true = a . b . a, false = a . b . b",
    "yCombinator x" ->
      """the keyword `yCombinator` allows for recursion, duplicating the expression on the righthand side
        |and infinitely replacing it on the lefthand side, i.e. yCombinator = (f . (x . f (x x)) (x . f (x x)))""".stripMargin,
    "yCombinator (fac . n . ( (<= n 1) 1 (* n (fac (- n 1))))) 3" -> "yCombinator with the non-recursive part of the factorial function as an argument"
  )

  private val titleStyle = js.Dynamic.literal("display" -> "flex", "justifyContent" -> "center", "margin" -> "auto")
  private val snippetsStyle =
    js.Dynamic.literal("display" -> "flex", "flexDirection" -> "column", "justifyContent" -> "space-evenly")
  private val snippetItem =
    js.Dynamic.literal("flex"      -> "none",
                       "width"     -> "100%",
                       "height"    -> "auto",
                       "marginTop" -> "1em",
                       "padding"   -> "16px")

  def render(): ReactElement =
    div(
      div(style := titleStyle)(header(h1(className := "mdc-typography--headline1")("Welcome to Frase Web"))),
      div(style := Dynamic.literal("display" -> "flex", "justifyContent" -> "space-between"))(
        div(className := "mainPage")(
          h3(className := "mdc-typography--headline4")("Examples:"),
          div(style := snippetsStyle)(
            examples
              .map {
                case (program, explanation) =>
                  div(key := program, className := "mdc-card", style := snippetItem)(
                    code(program),
                    p(i(className := "mdc-typography--body2", style := Dynamic.literal("color" -> "#888"))(
                      explanation)))
              },
          ),
        ),
        div(className := "mainPage mdc-card", style := Dynamic.literal("padding" -> "0 2em 2em 2em"))(
          h3(className := "mdc-typography--headline4")("REPL"),
          Repl())
      )
    )
}
