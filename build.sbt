enablePlugins(ScalaJSPlugin)

name := "boxes-scalajs"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.7"

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.0"

libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.5.5"

scalaJSUseRhino in Global := false
