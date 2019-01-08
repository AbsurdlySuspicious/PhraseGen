name := "patnmgen"
version := "0.1"
scalaVersion := "2.12.8"

mainClass in assembly := None
assemblyJarName in assembly := "patnmgen.jar"
test in assembly := {}

libraryDependencies ++= Seq(
  "net.sf.extjwnl" % "extjwnl" % "2.0.0"
)
