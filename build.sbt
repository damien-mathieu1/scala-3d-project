val scala3Version = "3.7.0"

lazy val root = project
  .in(file("."))
  .enablePlugins(ScalaJSPlugin, ScalaJSBundlerPlugin)
  .settings(
    name             := "image3d",
    version          := "0.1.0-SNAPSHOT",
    scalaVersion     := scala3Version,
    scalaJSUseMainModuleInitializer := true,
    libraryDependencies ++= Seq(
      "dev.cheleb"    %%% "threesjs"    % "0.0.1",
      "org.scala-js"  %%% "scalajs-dom" % "2.8.0",
      "org.typelevel" %%% "cats-core"   % "2.12.0"
    ),
    Compile / npmDependencies ++= Seq(
      "three" -> "0.177.0"
    ),
    webpackBundlingMode := BundlingMode.Application
  )
