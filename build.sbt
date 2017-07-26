
val json4sNative = "org.json4s" %% "json4s-native" % "3.4.0"
val proj4j = "io.jeo" % "jeo" % "0.7"
val graphhopper = "com.graphhopper" % "graphhopper" % "0.7.0"
val kdtree = "com.thesamet" %% "kdtree" % "1.0.4"
val scalatic = "org.scalactic" %% "scalactic" % "3.0.0"
val scalatest = "org.scalatest" %% "scalatest" % "3.0.0" % "test"
//val http = "org.scalaj" % "scalaj-http_2.11" % "2.3.0"

def scalaSources(base: File): PathFinder = (base / "src") ** "*.scala"
//
val akkaV = "2.5.3"
val sprayV = "1.3.3"

enablePlugins(JavaServerAppPackaging)

lazy val root = (project in file(".")).
    settings(
        resolvers += DefaultMavenRepository,
        scalaVersion := "2.11.11",
        name := "beeline-routing",
        version := "1.0",
        // scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8"),
        libraryDependencies ++= Seq(
          json4sNative,
          proj4j,
          graphhopper,
          kdtree,
          scalatic,
          scalatest
//          http
        ) ++ Seq(
            // "io.spray"            %%  "spray-can"     % sprayV,
            // "io.spray"            %%  "spray-routing" % sprayV,
            // "io.spray"            %%  "spray-httpx" % sprayV,
            // "io.spray"            %%  "spray-testkit" % sprayV  % "test",
            "com.typesafe.akka"   %% "akka-http" % "10.0.9",
            "com.typesafe.akka" %% "akka-http-spray-json" % "10.0.9",
            "com.typesafe.akka"   %%  "akka-actor"    % akkaV,
            "com.typesafe.akka"   %%  "akka-testkit"  % akkaV   % "test",
            "org.specs2"          %%  "specs2-core"   % "2.3.11" % "test",
            "org.zeromq" % "jeromq" % "0.3.5",
            "org.scala-lang.modules" % "scala-xml_2.11" % "1.0.5"
        )
    )


fork in run := true
