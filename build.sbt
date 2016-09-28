
val json4sNative = "org.json4s" %% "json4s-native" % "3.4.0"
val proj4j = "io.jeo" % "jeo" % "0.7"
val graphhopper = "com.graphhopper" % "graphhopper" % "0.7.0"
val kdtree = "com.thesamet" %% "kdtree" % "1.0.4"
val scalatic = "org.scalactic" %% "scalactic" % "3.0.0"
val scalatest = "org.scalatest" %% "scalatest" % "3.0.0" % "test"

def scalaSources(base: File): PathFinder = (base / "src") ** "*.scala"

lazy val root = (project in file(".")).
    settings(
        name := "intelligent-routing",
        version := "1.0",
        libraryDependencies ++= Seq(
          json4sNative,
          proj4j,
          graphhopper,
          kdtree,
          scalatic,
          scalatest
        ) ++ Seq(
            "io.spray"            %%  "spray-can"     % sprayV,
            "io.spray"            %%  "spray-routing" % sprayV,
            "io.spray"            %%  "spray-httpx" % sprayV,
            "io.spray"            %%  "spray-testkit" % sprayV  % "test",
            "com.typesafe.akka"   %%  "akka-actor"    % akkaV,
            "com.typesafe.akka"   %%  "akka-testkit"  % akkaV   % "test",
            "org.specs2"          %%  "specs2-core"   % "2.3.11" % "test"
        )
    )

fork in run := true
