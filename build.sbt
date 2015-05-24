name := "CourseraML"

version := "1.0"

scalaVersion := "2.11.6"

resolvers ++= Seq(
    "Typesafe" at "http://repo.typesafe.com/typesafe/releases/",
    "Akka Snapshot Repository" at "http://repo.akka.io/snapshots/",
    "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
)


libraryDependencies ++= Seq(

    /* dependencies for matrix, ML and NLP */
    "org.scalanlp" %% "breeze" % "0.11.2"
    // native libraries are not included by default. add this if you want them (as of 0.7)
    // native libraries greatly improve performance, but increase jar sizes.
//    "org.scalanlp" %% "breeze-natives" % "0.11.2"
)