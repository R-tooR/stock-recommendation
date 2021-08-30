package data

import data.queries.Query
import org.apache.spark.SparkConf
import org.apache.spark.sql.{DataFrame, DataFrameReader, SparkSession}

import java.util.Properties
import scala.collection.mutable

class DataExtractor(val props: Properties) {
  // http://spark.apache.org/docs/latest/configuration.html#configuring-logging
  // http://spark.apache.org/docs/latest/configuration.html
  // https://neo4j.com/developer/spark/configuration/
  // https://neo4j.com/docs/#driver-configuration

  private val config: SparkConf = {
    val params = mutable.Map(
      "appname" -> props.getProperty("sparkConf.appname"),
      "master" -> props.getProperty("sparkConf.master"),
      "spark.logConf" -> props.getProperty("sparkConf.spark.logConf"),
    )
    if (params.values.exists(_ == null)){
      params("sparkConf.appname") = "Recommendation Engine"
      params("sparkConf.master") = "local"
      params("sparkConf.spark.logConf") = "true"
    }
    new SparkConf().setAppName(props.getProperty("sparkConf.appname", "My App"))
      .setMaster(props.getProperty("sparkConf.master", "local"))
      .set("sparkConf.spark.logConf", props.getProperty("sparkConf.spark.logConf", "true"))
  }
//    .set("spark.driver.log.layout", "%d{yy/MM/dd HH:mm:ss.SSS} %t %p %c{1}: %m%n")
//    .set("spark.driver.log.level", "WARNING")
// http://spark.apache.org/docs/latest/configuration.html#viewing-spark-properties
  private val spark: SparkSession = SparkSession.builder().config(config).getOrCreate()

  val connection: DataFrameReader = {
    val params = mutable.Map(
      "url" -> props.getProperty("dfReader.url"),
      "username" -> props.getProperty("dfReader.username"),
      "password" -> props.getProperty("dfReader.password"),
    )

    if (params.values.exists(_ == null)){
      params("url") = "bolt://localhost:7687"
      params("username") = "neo4j"
      params("password") = "inv"
    }
    spark.read.format("org.neo4j.spark.DataSource")
      .option("url", params("url"))
      .option("authentication.basic.username", params("username"))
      .option("authentication.basic.password",  params("password"))
  }

  def get(query: Query): DataFrame = {
    connection
      .option("query", query.get)
      .load()
  }

  private def initializeWithParameters(prefix: String, paramNamesWithDefaults: Map[String, String]) = {
    val params = for ((k, v) <- paramNamesWithDefaults) yield (k, props.getProperty(prefix + "." + k))
    if (params.values.exists(_ == null)) paramNamesWithDefaults else params
  }
}
