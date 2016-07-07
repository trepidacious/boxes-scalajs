enablePlugins(ScalaJSPlugin)

name := "boxes-scalajs"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.7"

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.0"

libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.5.5"

libraryDependencies += "com.github.japgolly.fork.monocle" %%% "monocle-core" % "1.2.0"

libraryDependencies += "com.github.japgolly.fork.monocle" %%% "monocle-generic" % "1.2.0"

libraryDependencies += "com.github.japgolly.fork.monocle" %%% "monocle-state" % "1.2.0"

libraryDependencies += "com.github.japgolly.fork.monocle" %%% "monocle-macro" % "1.2.0"

scalaJSUseRhino in Global := false
