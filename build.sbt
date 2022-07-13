lazy val Scala213               = "2.13.8"
lazy val Scala212               = "2.12.15"
lazy val Scala3                 = "3.0.0"
lazy val supportedScalaVersions = List(Scala213)

ThisBuild / scalaVersion := Scala213
ThisBuild / organization := "io.github.kitlangton"
ThisBuild / organizationName := "kitlangton"
ThisBuild / description := "A form combinator library for decimating frontend boilerplate"
ThisBuild / homepage := Some(url("https://github.com/kitlangton/formula"))
name := "formula"

Global / onChangedBuildSource := ReloadOnSourceChanges

val sharedSettings = Seq(
  licenses := Seq("APL2" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
//  semanticdbEnabled := true, // enable SemanticDB
//  semanticdbVersion := scalafixSemanticdb.revision, // use Scalafix compatible version
  developers := List(
    Developer(
      id = "kitlangton",
      name = "Kit Langton",
      email = "kit.langton@gmail.com",
      url = url("https://github.com/kitlangton")
    )
  ),
  libraryDependencies ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, n)) if n <= 12 =>
        List(compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full))
      case _                       =>
        List()
    }
  },
  Compile / scalacOptions ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, n)) if n <= 12 => List("-Ypartial-unification")
      case _                       => List("-Ymacro-annotations", "-Ywarn-unused", "-Wmacros:after")
    }
  }
)

lazy val root = project
  .in(file("."))
  .aggregate(core, examples)
  .settings(
    publish / skip := true
  )

lazy val core = project
  .in(file("core"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name := "formula",
    scalacOptions ++= Seq("-Ymacro-annotations"),
    libraryDependencies ++= Seq(
      "com.raquo"                    %%% "laminar"       % "0.14.2",
      "com.softwaremill.magnolia1_2" %%% "magnolia"      % "1.1.2",
      "org.scala-lang"                 % "scala-reflect" % scalaVersion.value % Provided
    ),
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) },
    scalaJSLinkerConfig ~= { _.withSourceMap(false) }
  )
  .settings(sharedSettings)

lazy val examples = project
  .in(file("examples"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name := "formula-examples",
    libraryDependencies ++= Seq(
      "com.raquo"         %%% "laminar"              % "0.14.2",
      "io.github.cquiroz" %%% "scala-java-time"      % "2.4.0",
      "io.github.cquiroz" %%% "scala-java-time-tzdb" % "2.4.0"
    ),
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) },
    scalaJSLinkerConfig ~= { _.withSourceMap(false) },
    scalaJSUseMainModuleInitializer := true,
    publish / skip := true
  )
  .settings(sharedSettings)
  .dependsOn(core)
