package recommendation.recommender

import org.apache.commons.math3.linear.{Array2DRowRealMatrix, RealMatrix}
import org.apache.spark.sql.Row
import recommendation.engine.Engine

import java.util
import scala.collection.mutable

abstract class Recommender {
  def step(stockRecommendations: util.Map[String, Double], targetInvestor: Int): List[(String, Double)] = {
    val stocksByInvestors = findStocksBaseOnSimilarity(targetInvestor)

    stocksByInvestors._1.map(x => (x._1, stockRecommendations.getOrDefault(x._1, 0.0) + x._2)).toList
      .sorted(Ordering.by[(String, Double), Double](_._2).reverse)
  }

  def findStocksBaseOnSimilarity(targetInvestor: Int, evaluation: (Double, Double) = (0.0, 0.0)): (mutable.Map[String, Double], mutable.Map[String, Double])


  def getTopNStocks(similarities: Seq[Double], theirCompanies: Iterable[Row], evaluation: (Double, Double) = (0.0, 0.0)) = { // , recommendedRelevantStocks: mutable.Map[String, Double] = mutable.Map[String, Double]()
    val recommendedStocks = mutable.Map[String, Double]()
    val recommendedStocksCount = mutable.Map[String, Int]()

    def addIfNotExist(key: String, value: Double, recommendedStocks: mutable.Map[String, Double]) = {
      if (recommendedStocks contains key) {
        recommendedStocks(key) += value
      } else {
        recommendedStocks(key) = value
      }
    }

    theirCompanies zip similarities.tail foreach (x => {
      x._1.getList(0).forEach((cp: String) => addIfNotExist(cp, x._2, recommendedStocks))
    })

    val recommendedRelevantStocks = mutable.Map[String, Double]()
    if(evaluation._1 > 0.0 && evaluation._2 > 0.0) {
      (similarities.tail zip similarities.indices.toList)
        .sorted(Ordering.by[(Double, Int), Double](_._1).reverse)
        .take((similarities.size.toDouble * evaluation._1).toInt)
        .foreach(inv => {
          theirCompanies.toVector(inv._2).getList(0).forEach((cp: String) => addIfNotExist(cp, 1.0, recommendedRelevantStocks))
        })
      println("Relevant stocks: " + recommendedRelevantStocks.filter(x => x._2 >= evaluation._2))
    }

    println("Similar count: " + recommendedStocksCount)
    (recommendedStocks, recommendedRelevantStocks.filter(x => x._2 >= evaluation._2))
  }

  protected def handleResult(engine: Engine, data: List[RealMatrix], stocks: RealMatrix => (mutable.Map[String, Double], mutable.Map[String, Double])) = {
    engine.createConsensusEmbedding(data) match {
      case Right(matrix) => stocks(matrix)
      case Left(error) =>
        println(error)
        (mutable.Map[String, Double](), mutable.Map[String, Double]())
    }
  }

  protected def normalize = (d: Double) => 1 - (1/((0.1*d) + 1))

  protected def collection2DToRealMatrix(nested: Iterable[Iterable[Double]]): Array2DRowRealMatrix = {
    val doubleArray = nested map(iter => iter.toArray) toArray

    new Array2DRowRealMatrix(doubleArray)
  }
}
