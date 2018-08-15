val scalatest = "org.scalatest" %% "scalatest" % "3.0.5" % "test"

def scalaSources(base: File): PathFinder = (base / "src") ** "*.scala"
//

val commonSettings = Seq(
  resolvers += DefaultMavenRepository,
  scalaVersion := "2.12.6",
  version := "1.0"
)

lazy val fuzzyClustering = (project in file("fuzzy-clustering")).
  settings(
    commonSettings,
    name := "fuzzy-clustering",
    libraryDependencies ++= Seq(
      scalatest
    ),
    mainClass in assembly := Some("sg.beeline.clustering.FuzzyDistanceClusterApp")
  )

lazy val routing = (project in file("routing"))
  .settings(
    commonSettings,
    name := "routing",
    libraryDependencies ++= Seq(
      "io.jeo" % "jeo" % "0.7",
      "com.graphhopper" % "graphhopper" % "0.7.0",
      "com.thesamet" %% "kdtree" % "1.0.5",
      "org.scalactic" %% "scalactic" % "3.0.0",
      scalatest,
      "org.postgresql" % "postgresql" % "42.1.3",
      "com.typesafe.akka"   %% "akka-http" % "10.1.0",
      "com.typesafe.akka"   %%  "akka-http-testkit"  % "10.1.0" % "test",
      "com.typesafe.akka"   %%  "akka-actor"    % "2.5.13",
      "com.typesafe.akka"   %%  "akka-testkit"  % "2.5.13"   % "test",
      "com.typesafe.akka"   %% "akka-stream" % "2.5.13",

      // for CORS support
      "ch.megard" %% "akka-http-cors" % "0.3.0",

      // for database support
      "com.typesafe.slick" %% "slick" % "3.2.1",
      "com.typesafe.slick" %% "slick-hikaricp" % "3.2.1",

      "io.circe" %% "circe-literal" % "0.8.0",
      "io.circe" %% "circe-parser" % "0.8.0",
      "io.circe" %% "circe-generic-extras" % "0.8.0" // -generic-extras allows for default values
    ),
    parallelExecution in test := false
  )

lazy val root = (project in file("."))
  .aggregate(fuzzyClustering, routing)
  .settings(
    commonSettings,
    name := "beeline-routing",
    mainClass in assembly := Some("sg.beeline.BeelineRoutingApp")
  )

fork in run := true

