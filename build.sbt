name := "patnmgen"
version := "0.1"
scalaVersion := "2.12.8"

mainClass in assembly := Some("patnmgen.Patnmgen")
assemblyJarName in assembly := "patnmgen.jar"
test in assembly := {}

libraryDependencies ++= Seq(
  "com.github.scopt" %% "scopt" % "4.0.0-RC2",
  "net.sf.extjwnl" % "extjwnl" % "2.0.0",
  "org.scalatest" %% "scalatest" % "3.0.5" % Test
)
