package com.lambdaminute.frase.web

import slinky.core._
import slinky.core.annotations.react
import slinky.web.html._

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

@JSImport("resources/App.css", JSImport.Default)
@js.native
object AppCSS extends js.Object

@JSImport("resources/logo.svg", JSImport.Default)
@js.native
object ReactLogo extends js.Object

@react class App extends StatelessComponent {
  type Props = Unit

  private val css = AppCSS

  def render() =
    div(className := "App")(
      header(className := "App-header")(
        h1(className := "App-title")("Welcome to Frase Web")
      ),
      p(className := "App-intro")(
        "Enter some lambda calculus in the text field to see the beta-reduction"
      ),
      Repl()
    )
}
