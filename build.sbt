lazy val root = (project in file("."))
  .settings(
    name := "billiard-system-3d",
    version := "0.1",
    scalaVersion := "2.13.4",
    scalaSource in Test := baseDirectory.value / "src/test/scala",
    scalaSource in Compile := baseDirectory.value / "src/main/scala"
  )

libraryDependencies ++= Seq(
  // Lin. alg. library
  "org.scalanlp" %% "breeze" % "1.1",
  "org.scalanlp" %% "breeze-natives" % "1.1",
  "org.scalanlp" %% "breeze-viz" % "1.1",

  // Unit test library
  "org.scalatest" %% "scalatest" % "3.1.1" % "test"
)