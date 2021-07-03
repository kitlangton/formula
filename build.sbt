name := "formula"

version := "0.1"

val sharedSettings =
  Seq(
    scalaVersion := "2.13.6"
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
    scalacOptions ++= Seq("-Ymacro-annotations"),
    libraryDependencies ++= Seq(
      "dev.zio"        %%% "zio"           % "1.0.9",
      "com.raquo"      %%% "laminar"       % "0.13.0",
      "com.propensive" %%% "magnolia"      % "0.17.0",
      "org.scala-lang"   % "scala-reflect" % scalaVersion.value % Provided
    ),
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) },
    scalaJSLinkerConfig ~= { _.withSourceMap(false) }
  )
  .settings(sharedSettings)

lazy val examples = project
  .in(file("examples"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    libraryDependencies ++= Seq("com.raquo" %%% "laminar" % "0.13.0"),
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) },
    scalaJSLinkerConfig ~= { _.withSourceMap(false) },
    scalaJSUseMainModuleInitializer := true,
    publish / skip := true
  )
  .settings(sharedSettings)
  .dependsOn(core)
