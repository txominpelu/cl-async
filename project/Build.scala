import sbt._ 
import Keys._

object BuildSettings {
  val buildSettings = Defaults.defaultSettings ++ Seq(
    organization := "org.scalamacros",
    version := "1.0.0",
    scalaVersion := "2.11.0-M7",
    resolvers +=
      "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
  )
}

object MyBuild extends Build {
  import BuildSettings._

  def excludeM6Modules(m: ModuleID) = (m
    exclude("org.scala-lang.modules", "scala-parser-combinators_2.11.0-M6")
    exclude("org.scala-lang.modules", "scala-xml_2.11.0-M6")
  )

  // include these settings in your project:
  lazy val root: Project = Project(
    "root",
    file("."),
    settings = buildSettings
  ) aggregate(macros, core)


  lazy val macros: Project = Project(
    "macros",
    file("macros"),
    settings = buildSettings ++ Seq(
      libraryDependencies += excludeM6Modules("org.scala-lang" % "scala-compiler" % scalaVersion.value),
      libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.0-RC7",
      libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.0-RC5",
      libraryDependencies += excludeM6Modules("org.scala-lang" % "scala-reflect" % scalaVersion.value), 
      libraryDependencies ++= Seq(
        "org.apache.avro" % "avro" % "1.7.3",
        "org.specs2" % "specs2_2.11.0-M7" % "2.3.6" % "test"
      )

    ) ++ net.virtualvoid.sbt.graph.Plugin.graphSettings
  )

  lazy val core: Project = Project(
    "core",
    file("core"),
    settings = buildSettings ++ net.virtualvoid.sbt.graph.Plugin.graphSettings
  ) dependsOn(macros)
}
