name := "dartkreader"
organization := "dk.au.cs"
version := "0.1.0"
scalaVersion := "2.11.8"

libraryDependencies += "org.json4s" % "json4s-native_2.11" % "3.4.0"
libraryDependencies += "org.json4s" % "json4s-jackson_2.11" % "3.4.0"
libraryDependencies += "com.lihaoyi" %% "pprint" % "0.4.1"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

publishTo := Some(
  "Sonatype Snapshots Nexus" at "https://oss.sonatype.org/content/repositories/snapshots")
credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")

// Publish locally
/*
publishTo := Some(
  Resolver.file("file",
                new File(Path.userHome.absolutePath + "/.m2/repository")))
 */
