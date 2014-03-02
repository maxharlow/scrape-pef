name := "company-detailer"

scalaVersion := "2.10.3"

resolvers ++= Seq(
  "Anormcypher Repository" at "http://repo.anormcypher.org/"
)

libraryDependencies ++= Seq(
  "org.scalaj" %% "scalaj-http" % "0.3.14",
  "org.json4s" %% "json4s-native" % "3.2.7",
  "org.anormcypher" %% "anormcypher" % "0.4.4"
)
