name := "billiard-system-3d"

version := "0.1"

scalaVersion := "2.13.4"

libraryDependencies  ++= Seq(
  // Lin. alg. library
  "org.scalanlp" %% "breeze" % "1.1",
  "org.scalanlp" %% "breeze-natives" % "1.1",
  "org.scalanlp" %% "breeze-viz" % "1.1",

  // Unit test library
  "org.scalatest" %% "scalatest" % "3.1.1" % "test"
)