name := "plutolatry"

scalaVersion := "2.10.3"

scalaSource in Compile := baseDirectory.value / "src"

resolvers ++= Seq(
  "Anormcypher Repository" at "http://repo.anormcypher.org/"
)

libraryDependencies ++= Seq(
  "com.github.tototoshi" %% "scala-csv" % "1.0.0",
  "org.scalaj" %% "scalaj-http" % "0.3.14",
  "org.json4s" %% "json4s-native" % "3.2.7",
  "org.anormcypher" %% "anormcypher" % "0.4.4"
)