import sbt._
import Keys._

/**
  * fpInScala by chapter.
  *
  * Created by Paul Oglesby on 11/03/2016.
  */
object FPInScalaBuild extends Build {

  val compileTest = "compile->compile;test->test"

  /** Module projects; not configured for deployment as jar */

  lazy val root = Project(
    "fpInScala",
    file("."),
    settings = scalaProjectSettings
  )

  /** Dependencies */

  def scalaVersionString = "2.11.7"
  def scalaBinaryVersionString = "2.11"

  def scalaTest = "org.scalatest" %% "scalatest" % "2.2.5" % "test"
  def scalaCheck = "org.scalacheck" %% "scalacheck" % "1.12.5" % "test"

  lazy val sharedScalaDependencies = Seq(
    "org.scala-lang" % "scala-reflect" % scalaVersionString,
    "org.scala-lang.modules" % "scala-xml_2.11" % "1.0.5",
    scalaTest intransitive,
    scalaCheck intransitive
  )

  /** Settings */

  def projectSettings = Seq(
    organization := "ly.analogical",
    scalaBinaryVersion := scalaBinaryVersionString,
    scalaVersion := scalaVersionString,
    moduleConfigurations := Seq(),
    retrieveManaged := true,
    concurrentRestrictions in Global += Tags.limit(Tags.Test, 1),
    transitiveClassifiers in Scope.GlobalScope := Seq("sources"),
    ivyLoggingLevel := UpdateLogging.Quiet,
    crossPaths := false,
    publishMavenStyle := true,
    publishArtifact in (Compile, packageDoc) := false,
    publishArtifact in (Test, packageDoc) := false,
    javacOptions := Seq("-source", "1.8", "-target", "1.8", "-encoding", "utf8"),
    javaOptions := Seq("-server", "-XX:ReservedCodeCacheSize=192m", "-Xss2m"),
    javaOptions in Test := Seq("-server", "-Xmx2g", "-XX:ReservedCodeCacheSize=192m", "-Xss2m"),
    testFrameworks := Seq(TestFrameworks.ScalaTest),
    noTestCompletion(),
    scalacOptions := Seq(
      "-deprecation",
      "-optimize",
      "-unchecked",
      "-encoding", "utf8",
      "-target:jvm-1.8",
      "-Xlog-reflective-calls",
      "-feature",
      "-language:_"
    ) ++ Seq(
      "by-name-right-associative",
      "delayedinit-select",
      "doc-detached",
      "inaccessible",
      "missing-interpolator",
      "nullary-override",
      "option-implicit",
      "package-object-classes",
      "poly-implicit-overload",
      "private-shadow",
      "unsound-match"
    ).map(x => s"-Xlint:$x"),
    compileOrder := CompileOrder.JavaThenScala,
    fork in Test := true,
    testOptions in Test := Seq(Tests.Filter(testName => testName.endsWith("Test") || testName.endsWith("Tests"))),
    testOptions in Test += Tests.Argument("-oDF")
  )

  def scalaProjectSettings = projectSettings ++ Seq(
    libraryDependencies ++= sharedScalaDependencies
  )
  
}