name := "formula"

version := "0.1"

scalaVersion := "2.13.5"

lazy val core = project
  .in(file("."))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) },
    scalaJSLinkerConfig ~= { _.withSourceMap(false) },
    scalaJSUseMainModuleInitializer := true
  )

scalacOptions ++= Seq(
  "-Ymacro-annotations"
)

libraryDependencies ++= Seq(
  "com.raquo"      %%% "laminar"       % "0.12.1",
  "com.propensive" %%% "magnolia"      % "0.17.0",
  "org.scala-lang"   % "scala-reflect" % scalaVersion.value % Provided
)
