ThisBuild / scalaVersion     := "2.13.7"
ThisBuild / version          := "0.1.0"
ThisBuild / organization     := "edu.cornell"
ThisBuild / organizationName := "Cornell University"
ThisBuild / scalacOptions ++= Seq("-unchecked", "-deprecation")

lazy val root = (project in file("."))
  .settings(
    name := "bril-processing",
    resolvers += "Lcl" at f"file://${System.getProperty("user.home")}/.ivy2/local/default",
    libraryDependencies += "edu.cornell" %%  "bril-scala" % "0.1.0"
  )
