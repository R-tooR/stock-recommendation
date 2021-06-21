package data

import data.queries.Query
import org.apache.spark.SparkConf
import org.apache.spark.sql.{DataFrame, DataFrameReader, SparkSession}

import java.util.Properties

class DataExtractor(val props: Properties) {
  // http://spark.apache.org/docs/latest/configuration.html#configuring-logging
  // http://spark.apache.org/docs/latest/configuration.html
  private val config: SparkConf = new SparkConf().setAppName(props.getProperty("appname", "My App"))
    .setMaster(props.getProperty("master", "local"))
    .set("spark.logConf", "true")

  private val spark: SparkSession = SparkSession.builder().config(config).getOrCreate()

  val connection: DataFrameReader = spark.read.format("org.neo4j.spark.DataSource")
    .option("url", props.getProperty("url", "bolt://localhost:7687"))
    .option("authentication.basic.username", props.getProperty("username", "neo4j"))
    .option("authentication.basic.password", props.getProperty("password", "inv"))

  def get(query: Query): DataFrame = {
    connection
      .option("query", query.get)
      .load()
  }
}

