name := "lisp-interpreter"

version := "1.0"

scalaVersion := "2.12.2"

libraryDependencies ++= Seq(
  "org.parboiled" %% "parboiled" % "2.1.4",
  "org.typelevel" %% "cats"      % "0.9.0",
  "org.scalactic" %% "scalactic" % "3.0.1",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)
