import sbt.{Def, _}
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._

object Dependencies {

  object v {
    val softwaremillTagging = "2.2.0"
    val scalacheck          = "1.13.5"
    val parserCombinators   = "1.0.5"
    val scalatest           = "3.0.1"
    val cats                = "1.1.0"
    val catsEffect          = "1.0.0-RC"
    val shapeless           = "2.3.3"
  }

  val scalacheck: Def.Initialize[ModuleID] = Def.setting("org.scalacheck" %%% "scalacheck" % v.scalacheck % "test")
  val scalatest: Def.Initialize[ModuleID]  = Def.setting("org.scalatest"  %%% "scalatest"  % v.scalatest  % "test")

  val sharedTestDeps: Def.Initialize[Seq[ModuleID]] = Def.setting(
    Seq("org.scalacheck" %%% "scalacheck" % v.scalacheck % "test",
        "org.scalatest"  %%% "scalatest"  % v.scalatest  % "test"))

  val sharedDeps: Def.Initialize[Seq[sbt.ModuleID]] = Def.setting(
    Seq(
      "org.typelevel"          %%% "cats-macros"              % v.cats,
      "org.typelevel"          %%% "cats-kernel"              % v.cats,
      "org.typelevel"          %%% "cats-core"                % v.cats,
      "org.typelevel"          %%% "cats-effect"              % v.catsEffect,
      "org.scala-lang.modules" %%% "scala-parser-combinators" % v.parserCombinators,
      "com.chuusai"            %%% "shapeless"                % v.shapeless
    ))

}
