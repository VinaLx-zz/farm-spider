lazy val root = (project in file("."))
  .settings(
    inThisBuild(Seq(
      scalaVersion := "2.12.1")),
    libraryDependencies += "org.scalaj" %% "scalaj-http" % "2.3.0")