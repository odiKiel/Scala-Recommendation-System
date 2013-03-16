name := "Recommendation"

version := "1.0"

scalaVersion := "2.10.0"

libraryDependencies ++= Seq(
  "net.databinder.dispatch" %% "dispatch-core" % "0.9.5",
  "org.specs2" %% "specs2" % "1.12.3" % "test",
  "net.liftweb" %% "lift-json" % "2.5-M4",
  "com.twitter" %% "finagle-core" % "6.2.0",
  "com.twitter" %% "finagle-http" % "6.2.0",
  "com.typesafe.slick" %% "slick" % "1.0.0",
  "com.github.tototoshi" %% "scala-csv" % "0.7.0"
)
