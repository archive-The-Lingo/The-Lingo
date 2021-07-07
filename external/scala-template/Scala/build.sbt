ThisBuild / scalaVersion := "3.0.0"

lazy val root = project.in(file(".")).
  aggregate(foo.js, foo.jvm, foo.native).
  settings(
    publish := {},
    publishLocal := {},
  )

lazy val foo = crossProject(JSPlatform, JVMPlatform, NativePlatform).in(file(".")).
  settings(
    name := "foo",
    version := "0.1-SNAPSHOT",
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
