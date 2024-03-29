lazy val scala210 = "2.10.7"
lazy val scala211 = "2.11.12"
lazy val scala212 = "2.12.14"
lazy val scala213 = "2.13.6"
lazy val scala3 = "3.0.0"

ThisBuild / scalaVersion := scala213
ThisBuild / crossScalaVersions := Seq(scala210, scala211, scala212, scala213, scala3)

lazy val root = project.in(file(".")).
  aggregate(cross.js, cross.jvm, cross.native).
  settings(
    publish := {},
    publishLocal := {},
  )

libraryDependencies ++= Seq(
  "dev.zio" %% "izumi-reflect" % "1.1.3-RC1",
  "com.lihaoyi" %% "fastparse" % "2.3.2",

)

lazy val cross = crossProject(JSPlatform, JVMPlatform, NativePlatform).withoutSuffixFor(JVMPlatform).in(file(".")).
  settings(
    name := "foo",
    version := "0.1-SNAPSHOT",
    libraryDependencies ++= Seq(
      "dev.zio" %% "izumi-reflect" % "1.1.3-RC1",
      "com.lihaoyi" %% "fastparse" % "2.3.2",
    ),
  ).
  jvmSettings(
    // Add JVM-specific settings here
  ).
  jsSettings(
    // Add JS-specific settings here
    scalaJSUseMainModuleInitializer := true,
  ).
  nativeSettings(
    // Add Native-specific settings here
  )