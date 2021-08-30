import sbt.Keys.libraryDependencies

name := "stock-recommendation"

version := "0.1"

ThisBuild / scalaVersion := "2.12.13"
lazy val commonSettings = Seq(
  resolvers ++= Seq(
    ("Spark Packages Repo" at "http://dl.bintray.com/spark-packages/maven").withAllowInsecureProtocol(true),
    ("MavenRepository" at "https://mvnrepository.com")
  ),
  libraryDependencies ++= commonDependencies,
)

// unable to download neo4j-connector-apache-spark - not available on mvn repository: https://github.com/neo4j-contrib/neo4j-spark-connector/tree/v4.0.2
// consider building jar from sources and then adding it to classpath
lazy val recommendation = (project in file("recommendation-system"))
  .settings(
    commonSettings,
    libraryDependencies ++= Seq(
//      "neo4j-contrib" % "neo4j-connector-apache-spark_2.12" % "4.0.0",
//      "neo4j-contrib" % "neo4j-connector-apache-spark_2.12" % "4.0.1_for_spark_3",
      "org.apache.spark" % "spark-core_2.12" % "3.0.0",
      "org.apache.spark" % "spark-sql_2.12" % "3.0.0",
      "org.apache.spark" % "spark-mllib_2.12" % "3.0.0",
    )
  )

lazy val data = (project in file("data-provider"))
  .settings(
    commonSettings,
    libraryDependencies ++= Seq(
      "org.neo4j.driver" % "neo4j-java-driver" % "1.0.4",
      "com.fasterxml.jackson.module" % "jackson-module-scala" % "2.0.2"
    )
  )

lazy val commonDependencies = Seq(
  "org.scalactic" %% "scalactic" % "3.2.7",
  "org.scalatest" %% "scalatest" % "3.2.7" % "test",
  "org.apache.logging.log4j" %% "log4j-api-scala" % "11.0",
)
