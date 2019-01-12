package com.lambdaminute.frase.web

import slinky.core._
import slinky.core.annotations.react
import slinky.core.facade.ReactElement
import slinky.web.html._

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

  def render(): ReactElement =
    div(className := "App")(
      header(h1("Welcome to Frase Web")),
      h3("Examples:"),
      examples.map {
        case (program, explanation) =>
          div(key := program)(code(program), p(i(explanation), br(), br()))
      },
      hr(),
      Repl()
    )
}
