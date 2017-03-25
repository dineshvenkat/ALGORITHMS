import sbt.Keys._

name := "ALGORITHMS"

version := "1.0"

scalaVersion := "2.12.1"


scalaVersion := "2.11.7"
scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-language:_")

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.1" % "test"
)
    