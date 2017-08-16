import sbtcrossproject.CrossPlugin.autoImport.crossProject

lazy val util = (crossProject(JSPlatform, JVMPlatform, NativePlatform) in file("."))
  .settings(
    name := "util",
    organization := "com.avaglir",
    version := "0.1-SNAPSHOT",
    scalaVersion := "2.12.3",
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
    resolvers += Resolver.sonatypeRepo("releases"),
    scalacOptions ++= Seq(
      "-language:implicitConversions",
      "-language:reflectiveCalls",
      "-language:higherKinds"
    ),
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "com.lihaoyi" %%% "utest" % "0.4.8" % "test"
    ),
    scalacOptions in Test += "-Yrangepos",
    testFrameworks += new TestFramework("utest.runner.Framework")
  )
  .jvmSettings(
    libraryDependencies ++= Seq(
      "org.scala-js" %% "scalajs-stubs" % scalaJSVersion % "provided"
    )
  )
  .jsSettings(
  )
  .nativeSettings(
    scalaVersion := "2.11.11"
  )


lazy val js = util.js
lazy val jvm = util.jvm
lazy val native = util.native

lazy val root = (project in file("."))
  .aggregate(jvm, js, native)
