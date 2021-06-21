package data.processor

import org.apache.spark.sql.DataFrame

import scala.util.{Failure, Success, Try}

abstract class DataProcessor {
  def get(dfs: List[DataFrame]): Either[String, List[Iterable[Iterable[Double]]]]

  def tryInAttempts(tryCode: Try[List[Set[Nothing]]], attempts: Int = 2): List[Set[Nothing]] = {
    tryCode match {
      case Success(value) => value
      case Failure(exception) => {
        println("Parsing failed. Retrying... ")
        exception.printStackTrace()
        if (attempts > 0)
          tryInAttempts(tryCode, attempts - 1)
        else throw exception
      }
    }
  }
}
