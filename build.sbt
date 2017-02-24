lazy val root = (project in file("."))
  .settings(
    inThisBuild(Seq(
      scalaVersion := "2.12.1")),
    libraryDependencies ++= Seq(
      "org.scalaj" %% "scalaj-http" % "2.3.0",
      "org.json4s" %% "json4s-native" % "3.5.0"))