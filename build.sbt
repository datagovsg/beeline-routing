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
      "com.thesamet" %% "kdtree" % "1.0.5",
      "org.scalactic" %% "scalactic" % "3.0.0" % Test,
      scalatest,

      // for AWS
      "com.amazonaws" % "aws-java-sdk-lambda" % "1.11.385",

      "io.circe" %% "circe-literal" % "0.9.3",
      "io.circe" %% "circe-parser" % "0.9.3",
      "io.circe" %% "circe-generic-extras" % "0.9.3"
    ),
    parallelExecution in test := false,
    test in assembly := {},
    assemblyOption in assembly := (assemblyOption in assembly).value.copy(cacheOutput = false)
  )

lazy val routingOnMaps = (project in file("routing-on-maps"))
  .dependsOn(routing)
  .settings(
    commonSettings,
    name := "routingOnMaps",
    libraryDependencies ++= Seq(
      "com.graphhopper" % "graphhopper" % "0.7.0",
    )
  )

lazy val routingWeb = (project in file("routing-web"))
  .settings(
    commonSettings,
    name := "routing-web",
    libraryDependencies ++= Seq(
      scalatest,
      "org.scalactic" %% "scalactic" % "3.0.0",
      "org.postgresql" % "postgresql" % "42.1.3",

      "com.typesafe.akka"   %% "akka-stream" % "2.5.13",
      "com.typesafe.akka"   %% "akka-actor" % "2.5.13",
      "com.typesafe.akka"   %%  "akka-testkit"  % "2.5.13" % Test,
      "com.typesafe.akka"   %% "akka-http" % "10.1.0",
      "com.typesafe.akka"   %%  "akka-http-testkit"  % "10.1.0" % Test,

      // for CORS support
      "ch.megard" %% "akka-http-cors" % "0.3.0",

      "io.circe" %% "circe-literal" % "0.9.3",
      "io.circe" %% "circe-parser" % "0.9.3",
      // -generic-extras allows for default values
      "io.circe" %% "circe-generic-extras" % "0.9.3",

      "com.pauldijou" %% "jwt-circe" % "0.17.0",

      // for database support
      "com.typesafe.slick" %% "slick" % "3.2.1",
      "com.typesafe.slick" %% "slick-hikaricp" % "3.2.1"
    ),
    mainClass := Some("sg.beeline.web.RoutingApp"),
    test in assembly := {},
    assemblyOption in assembly := (assemblyOption in assembly).value.copy(cacheOutput = false)
  )
  .dependsOn(routing, routingOnMaps)

/**
  * This is the slimmed down package for AWS Lambda.
  * It does not contain:
  * 1) GraphHopper
  * 2) Akka Http / Actors
  * 3) Postgres
  */
lazy val routingLambda = (project in file("routing-lambda"))
  .dependsOn(routing)
  .settings(
    commonSettings,
    name := "routing-lambda",
    libraryDependencies ++= Seq(
      "io.github.mkotsur" %% "aws-lambda-scala" % "0.0.13",
      scalatest
    ),
    test in assembly := {},
    assemblyOption in assembly := (assemblyOption in assembly).value.copy(cacheOutput = false)
  )

lazy val root = (project in file("."))
  .aggregate(fuzzyClustering, routingWeb, routingLambda, routing, routingOnMaps)
  .settings(
    commonSettings,
    name := "beeline-routing",
    mainClass in assembly := Some("sg.beeline.BeelineRoutingApp")
  )

fork in run := true
cancelable in Global := true
