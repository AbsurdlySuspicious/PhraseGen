name := "phgen"
version := "1.0"
scalaVersion := "2.12.8"

mainClass in assembly := Some("phgen.Phgen")
assemblyJarName in assembly := "phgen.jar"
test in assembly := {}

libraryDependencies ++= Seq(
  "org.jline" % "jline" % "3.9.0",
  "com.github.scopt" %% "scopt" % "4.0.0-RC2",
  "org.scalatest" %% "scalatest" % "3.0.5" % Test
)
